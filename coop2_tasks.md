Analyzing Cooperation with Game Theory and Simulation
================================================================================
Task Set 2: Noisy Prisoners' Dilemma with Payments, Messages and Private Signals
================================================================================

**Date: 2013-11-25**
**Author: Sebastian Kranz (sebastian.kranz@uni-ulm.de)**


## New teams

  - We will randomly assign new teams for task 2 and put the teams on moodle. You can pick new team names.
  - On the Moodle site of the seminar, you find a file **coop2_vorlage.Rmd**, which you can download and open in RStudio. You should insert your solutions to all 3 scenarios and a description of your strategies in this file.


## Tentative time schedule for second task set

  - **Mo. 02.12.** Help Session 1 & Explanation of Extra-Task for Master Students
  - **Th. 05.12.** Please send us preliminary strategies for all three scenarios
  - **Mo. 09.12.** Help Session 2 and presentation of preliminary results
  - **Th. 12.12.** Please send us your final strategies for all scenarios
  - **Mo. 16.12.** Discussion of final strategies.
  - **Th. 09.01.** Send your answer strategies
  - **Mo. 13.01.** Presentation of final results

### Please install the newest version of StratTourn for this task

```r
library(devtools)
install_github(repo = "sktools", username = "skranz")
install_github(repo = "StratTourn", username = "skranz")
```



  
## Task 2 

Develop strategies for the three different scenarios explained below. All three scenarios use the following parameters:


```r
delta = 0.95
err.D.prob = 0.15
err.C.prob = 0
```





### Scenario 1: Noisy PD Game with Payments and Messages

In many real world relationships parties can communicate with each other and can also make monetary payments. We consider an extended version of the previous noisy prisoners' dilemma game, that accounts for these possibilities. 

#### An example strategy


```r

secret.code = function(obs, i, t, game, ...) {
    debug.store("secret.code", i = i, t = t)
    debug.restore("secret.code", i = 1, t = 2)
    
    
    # In t=0, cooperate, make a random payment, and send message with code.word
    # (There is abolutely no reason for this payment)
    if (t == 1) 
        return(nlist(a = "C", pay = round(runif(1, 0, 2), 1), m = list(code.word = 42)))
    
    a = obs$a  # observed actions
    pay = obs$pay  # previous payment
    m = obs$m  # previous messages
    prand = obs$prand  # a random number that is the same for both players
    
    j = 3 - i
    # Check if the other player sent a message with the correct code.word
    correct.code = FALSE
    if (has.field(m[[j]], "code.word", type = "numeric", length = 1)) {
        if (m[[j]]$code.word == 42) {
            correct.code = TRUE
        }
    }
    
    if (correct.code) {
        return(nlist(a = "C", pay = pay[j], m = list(code.word = 42)))
    } else {
        return(nlist(a = "D", pay = 0, m = list(code.word = 42)))
    }
}
```


A strategy must return the following objects:

  - an action a $\in$ {"C","D"}
  - a payment *pay >= 0* to the other player. A positive payment by player i will be simply subtracted from i's payoff and added to j's  payoff, i.e. players are risk-neutral.
  - a message *m* that can be a single variable or a list of different components. A message has no direct payoff impact, but may be used to exchange information.
  
The observations *obs* contain corresponding elements *obs$a*, *obs$m*, and *obs$pay*. Furthermore, players observe a variable *obs$prand*, which is just a random number between 0 and 1, which is the same for both players. (Game theorists call such a random number a *public randomization device*. It might be useful in some scenarios by allowing players to act in a coordinated fashion.)

A game object is generated with the following function call.

```r
library(StratTourn)
# Initialize game with private monitoring, by setting private.signals=TRUE
game = make.payment.message.pd.game(err.D.prob = err.D.prob)
```

Let us play secret.code 10 times against itself

```r
res = run.rep.game(strat = nlist(secret.code, secret.code), game = game, delta = delta, 
    T.min = 10, T.max = 10)
res
```

```
## $hist
##     t obs_a1 obs_a2 obs_pay1 obs_pay2 obs_m1 obs_m2 obs_prand a1 pay1 m1
## 1   1   <NA>   <NA>     <NA>     <NA>   <NA>   <NA>      <NA>  C  0.1 42
## 2   2      C      D      0.1      1.6     42     42      0.19  C  1.6 42
## 3   3      D      C      1.6      0.1     42     42     0.624  C  0.1 42
## 4   4      C      C      0.1      1.6     42     42     0.021  C  1.6 42
## 5   5      C      C      1.6      0.1     42     42     0.693  C  0.1 42
## 6   6      C      C      0.1      1.6     42     42     0.817  C  1.6 42
## 7   7      D      C      1.6      0.1     42     42     0.021  C  0.1 42
## 8   8      C      C      0.1      1.6     42     42     0.629  C  1.6 42
## 9   9      C      C      1.6      0.1     42     42     0.306  C  0.1 42
## 10 10      C      C      0.1      1.6     42     42     0.479  C  1.6 42
##    a2 pay2 m2  pi1  pi2
## 1   C  1.6 42  2.5 -0.5
## 2   C  0.1 42 -0.5  2.5
## 3   C  1.6 42  2.5 -0.5
## 4   C  0.1 42 -0.5  2.5
## 5   C  1.6 42  2.5 -0.5
## 6   C  0.1 42 -0.5  2.5
## 7   C  1.6 42  2.5 -0.5
## 8   C  0.1 42 -0.5  2.5
## 9   C  1.6 42  2.5 -0.5
## 10  C  0.1 42 -0.5  2.5
## 
## $u
## [1] 1.0385 0.9615
```

The output shows the history of the different actions *a*, messages *m* and payments *pay*.

#### Avoiding errors when handling messages

The strategy *secret.code* implements an obvious idea given that messages are possible: Each player uses their message to send a code word: m$code.word=42. If also the other player sends a message with the correct code word, the player cooperates and otherwise the player defects. (If the message is correct a player may also make a payment. There is no special reason for this, except to illustrate that payments are possible).

If your strategy interprets messages, you have to be careful, that your strategy does not create an error when it plays against a strategy that sends different messages. Consider the case that our strategy secret.code would just check for the code.word by typing:


```r
if (m[[j]]$code.word == 42) {
    correct.code = TRUE
}
```

Now assume it plays against a strategy that has a message m="hi". We would get an error, since the message has no field "code.word".

```r
m = list(list(code.word = 42), "hi")
j = 2
if (m[[j]]$code.word == 42) {
    correct.code = TRUE
}
```

```
## Error: $ operator is invalid for atomic vectors
```

For this purpose the function *has.field* exists that checks whether a message has a particular field. The function can also check whether that field is of a particular data type (you get the type of variable x, by typing class(x)) and length. Therefore we call has.field previous to other checks. It runs without error for all sorts of messages.

```r
m = list(list(code.word = 42), "hi")
j = 2
if (has.field(m[[j]], "code.word", type = "numeric", length = 1)) {
    if (m[[j]]$code.word == 42) {
        correct.code = TRUE
    }
}
```



#### Using messages for secret codes is a bit boring... 

While a strategy like secret.code probably looks like a winner in the first stage of the tournament, it won't remain stable once we run the second stage, where the competing teams see the code of your strategy and can device a best reply. For example, the following strategy is a simple best reply that extremely destabilizes *secret.code*:


```r
secret.code.basher = function(obs, i, t, game, ...) {
    return(nlist(a = "D", pay = 0, m = list(code.word = 42)))
}
```


```r
res = run.rep.game(strat = nlist(secret.code, secret.code.basher), game = game, 
    delta = 0.98, T.min = 5, T.max = 5)
res
```

```
## $hist
##   t obs_a1 obs_a2 obs_pay1 obs_pay2 obs_m1 obs_m2 obs_prand a1 pay1 m1 a2
## 1 1   <NA>   <NA>     <NA>     <NA>   <NA>   <NA>      <NA>  C  0.5 42  D
## 2 2      C      D      0.5        0     42     42     0.324  C    0 42  D
## 3 3      C      D        0        0     42     42     0.757  C    0 42  D
## 4 4      C      D        0        0     42     42     0.602  C    0 42  D
## 5 5      C      D        0        0     42     42     0.969  C    0 42  D
##   pay2 m2  pi1 pi2
## 1    0 42 -1.5 2.5
## 2    0 42 -1.0 2.0
## 3    0 42 -1.0 2.0
## 4    0 42 -1.0 2.0
## 5    0 42 -1.0 2.0
## 
## $u
## [1] -1.104  2.104
```





In a sense, secret codes are boring in our setting, since your strategies will be made known to all other teams. Of course, you could write your strategy in a fashion that is very hard to understand for other teams, so that it will be complicated to understand your secret code. However, you are asked to describe your strategy very well, and to make it easy to understand it for everybody. Since this is at odds with hidding somewhere a secret code, we have the following guide line:

    **Try not to win by hiding secret code phrases that strategies use to identify each other. Describe your strategy as clearly as possible. Don't try to win by hoping that other teams don't understand your strategy!**

#### If I shall not hide secret codes, for what do I need messages?

Actually, I don't believe that messages are useful in Scenario 1. However, messages may potentially be quite useful in Scenario 2. 

#### What about the payments, do I need them for anything?

Try out yourself whether payments can be useful to improve strategies...


### Scenario 2: Noisy PD Game with Private Monitoring, Payments and Messages

In all games and scenarios considered so far, both players always made the same observations. Even though observations may not always correspond to the actual actions, the observation errors were the same for both players. That information structure is called "imperfect public monitoring". Yet, in many real world relationships parties do not neccessarily always make the same observations and one often does not know what the other party has observed. Games in which observations are noisy and can differ between players are called games with **private monitoring**. 

In Scenario 2 we study a repeated prisoners' dilemma game with private monitoring. As before player i observes with probability *err.D.prob* that the other player j has played "D", even if in fact "C" was played. Yet, her own action is now always correctly observed by player i. The observation errors of player i and j are *independently distributed* from each other. This means a player j no longer knows whether player i has observed player j's action correctly, and vice versa. The actual observations of i and j can now differ.

I want to exemplify the information structure with a simple example with two strategies that always cooperate and don't use payments or messages.


```r
library(StratTourn)
# Initialize game with private monitoring, by setting private.signals=TRUE
game = make.payment.message.pd.game(err.D.prob = 0.2, private.signals = TRUE)

# A simple strategy that always cooperates sends no message and makes no
# payment
coop.strat = function(obs, i, t, game, ...) {
    return(nlist(a = "C", pay = 0, m = ""))
}

# Run coop.strat vs coop.strat and analyze output Note that I fix the number
# of rounds to 10 and provide a game.seed parameter which causes the random
# number generator to always return the same results.
res = run.rep.game(strat = nlist(coop.strat, coop.strat), game = game, delta = 0.98, 
    T.min = 10, T.max = 10, game.seed = 12345)
res
```

```
## $hist
##     t obs_pay1 obs_pay2 obs_m1 obs_m2 obs_prand obs1_a1 obs1_a2 obs2_a1
## 1   1     <NA>     <NA>   <NA>   <NA>      <NA>    <NA>    <NA>    <NA>
## 2   2        0        0                   0.456       C       D       C
## 3   3        0        0                   0.509       C       C       C
## 4   4        0        0                   0.035       C       D       C
## 5   5        0        0                   0.001       C       C       C
## 6   6        0        0                   0.388       C       C       D
## 7   7        0        0                   0.952       C       C       C
## 8   8        0        0                   0.965       C       C       C
## 9   9        0        0                    0.39       C       C       C
## 10 10        0        0                   0.226       C       C       C
##    obs2_a2 a1 pay1 m1 a2 pay2 m2 pi1 pi2
## 1     <NA>  C    0     C    0      1   1
## 2        C  C    0     C    0      1   1
## 3        C  C    0     C    0      1   1
## 4        C  C    0     C    0      1   1
## 5        C  C    0     C    0      1   1
## 6        C  C    0     C    0      1   1
## 7        C  C    0     C    0      1   1
## 8        C  C    0     C    0      1   1
## 9        C  C    0     C    0      1   1
## 10       C  C    0     C    0      1   1
## 
## $u
## [1] 1 1
```

Columns 2-5 show the observations for payments, messages and the public signal, which are the same for both players. Columns 7-10 shows both players private observations about the actions.

```r
res$hist[, 7:10]
```

```
##    obs1_a1 obs1_a2 obs2_a1 obs2_a2
## 1     <NA>    <NA>    <NA>    <NA>
## 2        C       D       C       C
## 3        C       C       C       C
## 4        C       D       C       C
## 5        C       C       C       C
## 6        C       C       D       C
## 7        C       C       C       C
## 8        C       C       C       C
## 9        C       C       C       C
## 10       C       C       C       C
```

The columns obs1_a1 and obs1_a2 show the action pair observed by player 1 and columns obs2_a1 and obs2_a2 show the action pair observed by player 2. You see that in periods 2 and 4 player 1 has wrongly observed that player 2 has defected. Yet player 2 does not know that player 1 has observed this defection. Similarly, in period 6 player 2 wrongly observes a defection by player 1.

While one can in principle use messages to tell the other player about the observations one has made, the game in itself does not prevent lies. A player has no method to prove her observations to the other player.

### Scenario 3: Public monitoring with a possibility to manipulate signals

Another real world problem is that signals about behavior can sometimes be manipulated. One can try to make the own actions look good or sometimes sabotage the other parties appearance by making their actions look bad. Such possibilities can provide a severe challenge for cooperation. This scenario analyzes these issues in a simple setting. We are back to the case of imperfect public monitoring, like in Scenario 1, i.e. both players make the same observation. Yet, in addition to choosing an action a, payment pay and message m, players can choose to sabotage or not. This is decided by returning an action **sab** which can be TRUE (perform sabotage) or FALSE (no sabotage).

If player i chooses to sabotage (sab=TRUE), we have the following effects:

  - In the next period players always observe that player j has defected (D). This means sabotage makes the other  player always look like a defector.
  
  - Player i, who performs the sabotage, has to bear **costs of sabotage c=0.3**, which are directly reduced from i's payoff in the period in which the sabotage is conducted.

We generate a game with such a sabotage opportunity with the following function call

```r
# Generate game with sabotage option
game = make.payment.message.sabotage.pd.game(err.D.prob = err.D.prob, sabotage.cost = 0.3)
```


Here is an example run with a strategy that randomly sabotages.

```r
random.sabotage = function(obs, i, t, game, ...) {
    # Sabotages with 30% probability
    return(nlist(a = "C", pay = 0, m = "", sab = runif(1) <= 0.3))
}

run.rep.game(strat = c(random.sabotage, random.sabotage), game = game, delta = delta, 
    T.min = 10, T.max = 10, game.seed = 1235, strat.seed = 9876543)
```

```
## $hist
##     t obs_a1 obs_a2 obs_pay1 obs_pay2 obs_m1 obs_m2 obs_prand a1 pay1 m1
## 1   1   <NA>   <NA>     <NA>     <NA>   <NA>   <NA>      <NA>  C    0   
## 2   2      C      D        0        0                   0.839  C    0   
## 3   3      C      C        0        0                   0.745  C    0   
## 4   4      C      C        0        0                   0.955  C    0   
## 5   5      D      D        0        0                   0.629  C    0   
## 6   6      C      D        0        0                   0.914  C    0   
## 7   7      C      C        0        0                   0.847  C    0   
## 8   8      C      C        0        0                   0.861  C    0   
## 9   9      C      C        0        0                   0.794  C    0   
## 10 10      C      C        0        0                   0.205  C    0   
##     sab1 a2 pay2 m2  sab2 pi1 pi2
## 1   TRUE  C    0    FALSE 0.7 1.0
## 2  FALSE  C    0    FALSE 1.0 1.0
## 3  FALSE  C    0    FALSE 1.0 1.0
## 4  FALSE  C    0     TRUE 1.0 0.7
## 5   TRUE  C    0    FALSE 0.7 1.0
## 6  FALSE  C    0    FALSE 1.0 1.0
## 7  FALSE  C    0    FALSE 1.0 1.0
## 8  FALSE  C    0    FALSE 1.0 1.0
## 9  FALSE  C    0    FALSE 1.0 1.0
## 10 FALSE  C    0    FALSE 1.0 1.0
## 
## $u
## [1] 0.9322 0.9679
```

You see that player 1 sabotages in periods 1 and 5 and that consequently in periods 2 and 6 it is observed that player 2 has defected. Furthermore, the payoff of player 1 in the periods of sabotage is only 0.7 instead of 1. Similarly, player 2 sabotages in period 4 and in period 5 it is observed that player 1 defects. The observed defection of player 2 has nothing to do with sabotage but is simply a random observation error, caused by the positive err.D.prob.

Also note that **sabotage actions are not directly observable**.
