Analyzing Cooperation with Game Theory and Simulation
========================================================================
Task Set 1: Noisy Prisoner Dilemma
========================================================================

**Date: 2013-10-11**
**Author: Sebastian Kranz (sebastian.kranz@uni-ulm.de)**

### Task 1.1 Install software and work through the tutorial

First read the tutorial for the seminar and for the R package StratTourn, install the software and try out the examples in the tutorial.

### Task 1.2 Develop strategies for 4 scenarios of a Noisy Prisoners' Dilemma Game

In the PD game of the tutorial, players can perfectly observe the action that their partner has chosen in the previous period. This is a strong assumption. In many real world problems that share some structure with a PD game, it is not always easy to observe what the other player has done (think of environmental agreements to reduce polution or reduce quantity of fishing, or hard to observe work effort on joint projects) Resulting information problems may become a severe obstacle to efficient cooperation and strategies have to take them into account.

#### Noisy PD Game

In this task we consider noisy prisoners' dilemma games that add a simple form of information problem. There is an exogenously given probability *err.D.prob* that players observe that a player has chosen D in the previous period, even if in fact he has chosen C. Similarly, there can be a positive probability *err.C.prob* that a player's action is observed as C, even though he has chosen D. Formally, the period t+1 observation $obs_{i,t+1}$ of player i's period t action $a_{i,t}$ is distributed as follows:
\[
obs_{i,t+1}=
\begin{cases}
  a_{i,t} & \mbox{with prob 1-err.D.prob-err.C.prob}\\
  D & \mbox{with prob err.D.prob}\\
  C & \mbox{with prob err.C.prob}
\end{cases}
\]

Both players always make the same observations, i.e. each player knows what the other player has observed. That all players make the same noisy observations is called *imperfect public monitoring* in game theoretic language. (If there are no observation errors, we have *perfect monitoring*. If players could make different observations, we would have *private monitoring*)

Every time shall develop one strategy for each of the following 4 scenarios:

#### 1. Baseline scenario:

  * err.D.prob = 0.15
  * err.C.prob = 0
  * $\delta=0.95$
  
#### 2. High err.D.prob scenario:

  * err.D.prob = 0.3
  * err.C.prob = 0
  * $\delta=0.95$

#### 3. err.C.prob scenario:

  * err.D.prob = 0.3
  * err.C.prob = 0
  * $\delta=0.95$

#### 4. Low $\delta$ scenario:

  * err.D.prob = 0.15
  * err.C.prob = 0
  * $\delta=0.7$

For each scenario separately the strategies of all teams will compete in a tournament as described in Section 4 of the tutorial. Scores are computed with an **instability.factor of 20**.

#### Two stages of the tournament  
As described in Section 4 of the tutorial, the tournament will have a second stage. After we run the first stage, every team gets the source code of all strategies and has 1-2 weeks time to develop strategies that have the only goal to increase the instability of competing strategies. The final score of the initially submitted strategies will be computed after this second stage.

#### A code example for testing strategies




```r
library(StratTourn)

# All scenarios baseline scenario
err.D.prob = 0.15
err.C.prob = 0
delta = 0.95
# high err.D.prob scenario
err.D.prob = 0.3
err.C.prob = 0
delta = 0.95
# err.C.prob scenario
err.D.prob = 0.15
err.C.prob = 15
delta = 0.95
# low delta scenario
err.D.prob = 0.15
err.C.prob = 0
delta = 0.7

# Current scenario baseline scenario
err.D.prob = 0.15
err.C.prob = 0
delta = 0.95


game = make.pd.game(err.D.prob = err.D.prob, err.C.prob = err.C.prob)

# Pick a pair of strategies
strat = nlist(tit.for.tat, grim.trigger)

# Let the strategies play against each other
run.rep.game(delta = delta, game = game, strat = strat)
```

```
## $hist
##    obs_a1 obs_a2 a1 a2 pi1 pi2 coop_2
## 1    <NA>   <NA>  C  C   1   1   TRUE
## 2       C      D  D  D   0   0  FALSE
## 3       D      D  D  D   0   0  FALSE
## 4       D      D  D  D   0   0  FALSE
## 5       D      D  D  D   0   0  FALSE
## 6       D      D  D  D   0   0  FALSE
## 7       D      D  D  D   0   0  FALSE
## 8       D      D  D  D   0   0  FALSE
## 9       D      D  D  D   0   0  FALSE
## 10      D      D  D  D   0   0  FALSE
## 11      D      D  D  D   0   0  FALSE
## 12      D      D  D  D   0   0  FALSE
## 
## $u
## [1] 0.08333 0.08333
```

```r

# Init and run a tournament of several strategies against each other
strat = nlist(grim.trigger, tit.for.tat, always.defect, always.coop)
tourn = init.tournament(game = game, strat = strat, delta = delta, instability.factor = 20)
tourn = run.tournament(tourn = tourn, R = 10)
tourn
```

```
## 
## Tournament for Noisy PD (10 rep.)
## 
##               grim.trigger tit.for.tat always.defect always.coop
## grim.trigger         0.162       0.180         -0.05        1.84
## tit.for.tat          0.153       0.269         -0.05        1.15
## always.defect        0.100       0.100          0.00        2.00
## always.coop         -0.676       0.704         -1.00        1.00
## 
## Ranking with instability.factor = 20
##               rank   score efficiency instability
## grim.trigger     1   0.162      0.162       0.000
## always.defect    2   0.000      0.000       0.000
## tit.for.tat      3  -8.430      0.269       0.435
## always.coop      4 -19.000      1.000       1.000
```

