Analyzing Cooperation with Game Theory and Simulation
========================================================================
Task Set 1: Noisy Prisoner Dilemma
========================================================================

**Date: 2013-10-11**
**Author: Sebastian Kranz (sebastian.kranz@uni-ulm.de)**


### Forming teams, solving the tasks and handing in your solutions

  - Form teams of 2 persons (at most one 3-person team is allowed) and pick a team name. Send an email with your team name and the team members to martin.kies@uni-ulm.de until tommorow, **Tuesday, 22.10.**.
  - On the Moodle site of the seminar, you find a file **coop1_vorlage.Rmd**, which you can download and open in RStudio. You should insert your solutions to all 4 scenarios of Task 1.2 and a description of your strategies in this file.
  - If you have any programming questions, use the **Forum on Moodle**, where we are happy to answer your questions. Everybody else is also encouraged to provide helpful answers.  **The Forum should be about programming difficulties, you should not ask or answer about a clever strategy design.**


#### Time schedule for first task set

  - **Tu. 22.10.** Send an email with your team name and members to martin.kies@uni-ulm.de
  - **Mo. 28.10.** Help Session 1: We will just meet in the seminar room (220) and you can ask questions about how to implement certain strategies in R. Of course you can bring your notebooks.
  - **Th. 31.10.** send a preliminary version, of your solution based on coop1_vorlage.Rmd per email to sebastian.kranz@uni-ulm.de and martin.kies@uni-ulm.de. Your preliminary version should have a working strategy at least for Scenario 1.
  - **Mo. 4.11.** Help Session 2: We will just meet in the seminar room (220) and you can ask questions about how to implement certain strategies in R. Of course you can bring your notebooks. We also will explain how the second stage of the tournament will work.
  - **Th. 7.11.**  Send us your final solutions for all 4 scenarios including the description of the strategies. Make sure that all your strategies run without error!
  - **Mo. 11.11.** The tournament results (first stage) will be presented and  some teams are asked to present their strategies.
  - Afterwards we will move to the second stage of the tournament and introduce the next task.

  


### Task 1.1 Install software and work through the tutorial

First read the tutorial for the seminar and for the R package StratTourn, install the software and try out the examples in the tutorial. The Tutorial is the file "StratTourn Tutorial" on Moodle.

### Task 1.2 Develop strategies for 4 scenarios of a Noisy Prisoners' Dilemma Game

In the PD game of the tutorial, players can perfectly observe the action that their partner has chosen in the previous period. This is a strong assumption. In many real world problems that share some structure with a PD game, it is not always easy to observe what the other player has done (think of environmental agreements to reduce polution or reduce quantity of fishing, or hard to observe work effort on joint projects) Resulting information problems may become a severe obstacle to efficient cooperation and strategies have to take them into account.

#### Noisy PD Game

In this task we consider noisy prisoners' dilemma games that add a simple form of information problem. There is an exogenously given probability *err.D.prob* that players observe that a player has chosen D in the previous period irrelevant of his actual choice (which might as well be C). Similarly, there can be a positive probability *err.C.prob* that a player's action is observed as C, even though he has chosen D. Formally, the period t+1 observation $obs_{i,t+1}$ of player i's period t action $a_{i,t}$ is distributed as follows:
\[
obs_{i,t+1}=
\begin{cases}
  a_{i,t} & \mbox{with prob 1-err.D.prob-err.C.prob}\\
  D & \mbox{with prob err.D.prob}\\
  C & \mbox{with prob err.C.prob}
\end{cases}
\]

Both players always make the same observations, i.e. each player knows what the other player has observed. That all players make the same noisy observations is called *imperfect public monitoring* in game theoretic language. (If there are no observation errors, we have *perfect monitoring*. If players could make different observations, we would have *private monitoring*)

Every team shall develop strategies for each of the following 4 scenarios (one strategy per scenario):

#### 1. Baseline scenario:

  * err.D.prob = 0.15
  * err.C.prob = 0
  * $\delta=0.95$
  
#### 2. High err.D.prob scenario:

  * err.D.prob = 0.3
  * err.C.prob = 0
  * $\delta=0.95$

#### 3. err.C.prob scenario:

  * err.D.prob = 0.15
  * err.C.prob = 0.3
  * $\delta=0.95$

#### 4. Low $\delta$ scenario:

  * err.D.prob = 0.15
  * err.C.prob = 0
  * $\delta=0.5$


For each scenario separately the strategies of all teams will compete in a tournament as described in Section 4 of the tutorial. Scores are computed with the formula
\[
    score = efficiency - 2*instability - 20*instability^2
\]


#### Two stages of the tournament  
As described in Section 4 of the tutorial, the tournament will have a second stage. After we run the first stage, every team gets the source code of all strategies and has 1-2 weeks time to develop strategies that have the only goal to increase the instability of competing strategies. The final score of the initially submitted strategies will be computed after this second stage.



### A code example for testing strategies




```r
library(StratTourn)

# scenario 1: baseline
err.D.prob = 0.15
err.C.prob = 0
delta = 0.95
# scenario 2: high err.D.prob
err.D.prob = 0.3
err.C.prob = 0
delta = 0.95
# scenario 3: err.C.prob
err.D.prob = 0.15
err.C.prob = 0.3
delta = 0.95
# scenario 4: low delta
err.D.prob = 0.15
err.C.prob = 0
delta = 0.5


# Current scenario baseline scenario
err.D.prob = 0.15
err.C.prob = 0
delta = 0.95


game = make.pd.game(err.D.prob = err.D.prob, err.C.prob = err.C.prob)

# Pick a pair of strategies
strat = nlist(tit.for.tat, random.action)

# Let the strategies play against each other
run.rep.game(delta = delta, game = game, strat = strat)
```

```
## $hist
##    obs_a1 obs_a2 a1 a2 pi1 pi2
## 1    <NA>   <NA>  C  D  -1   2
## 2       D      D  D  C   2  -1
## 3       D      C  C  D  -1   2
## 4       D      D  D  D   0   0
## 5       D      D  D  D   0   0
## 6       D      D  D  D   0   0
## 7       D      D  D  C   2  -1
## 8       D      C  C  C   1   1
## 9       C      D  D  C   2  -1
## 10      D      C  C  C   1   1
## 11      C      C  C  C   1   1
## 12      C      C  C  C   1   1
## 13      C      C  C  C   1   1
## 14      C      C  C  D  -1   2
## 15      C      D  D  D   0   0
## 16      D      D  D  D   0   0
## 17      D      D  D  C   2  -1
## 18      D      D  D  D   0   0
## 19      D      D  D  C   2  -1
## 20      D      C  C  C   1   1
## 21      C      C  C  D  -1   2
## 22      D      D  D  D   0   0
## 23      D      D  D  C   2  -1
## 24      D      C  C  D  -1   2
## 25      C      D  D  D   0   0
## 26      D      D  D  C   2  -1
## 27      D      C  C  C   1   1
## 28      C      C  C  C   1   1
## 29      C      C  C  C   1   1
## 30      C      C  C  D  -1   2
## 31      C      D  D  C   2  -1
## 32      D      C  C  C   1   1
## 33      C      C  C  D  -1   2
## 34      C      D  D  D   0   0
## 35      D      D  D  C   2  -1
## 36      D      C  C  C   1   1
## 37      C      C  C  D  -1   2
## 38      C      D  D  C   2  -1
## 39      D      C  C  D  -1   2
## 40      C      D  D  D   0   0
## 41      D      D  D  C   2  -1
## 42      D      D  D  C   2  -1
## 43      D      C  C  C   1   1
## 44      C      C  C  C   1   1
## 45      D      C  C  D  -1   2
## 46      C      D  D  C   2  -1
## 47      D      C  C  D  -1   2
## 48      D      D  D  C   2  -1
## 49      D      D  D  D   0   0
## 50      D      D  D  D   0   0
## 51      D      D  D  D   0   0
## 52      D      D  D  D   0   0
## 53      D      D  D  D   0   0
## 54      D      D  D  C   2  -1
## 55      D      C  C  D  -1   2
## 56      D      D  D  C   2  -1
## 57      D      C  C  C   1   1
## 58      C      D  D  C   2  -1
## 59      D      D  D  C   2  -1
## 60      D      C  C  C   1   1
## 61      C      C  C  C   1   1
## 
## $u
## [1] 0.6557 0.3607
```

```r

# Init and run a tournament of several strategies against each other
strat = nlist(tit.for.tat, always.defect, always.coop)
tourn = init.tournament(game = game, strat = strat, delta = delta, score.fun = "efficiency - 2*instability -20*instability^2")
tourn = run.tournament(tourn = tourn, R = 10)
tourn
```

```
## 
## Tournament for Noisy PD (10 rep.)
## 
##               tit.for.tat always.defect always.coop
## tit.for.tat         0.233         -0.05        1.19
## always.defect       0.100          0.00        2.00
## always.coop         0.679         -1.00        1.00
## 
## Ranking with score = efficiency - 2*instability -20*instability^2
## 
##               rank   score efficiency instability u.average   best.answer
## always.defect    1   0.000      0.000       0.000     0.700 always.defect
## tit.for.tat      2  -4.652      0.233       0.447     0.459   always.coop
## always.coop      3 -21.000      1.000       1.000     0.226 always.defect
```


See also the file pdgame.R on moodle for example strategies and some code you can adapt and test your strategies.
