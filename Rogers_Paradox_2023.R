#Simulation function
simSocialData <- function(nAgents, rounds=10, trials = 25, k = 10, alpha = .9, beta = 1, Q0 = 0, theta = 5){
  socialSimDF <- data.frame() #Initialize dataframe
  for (socLearners in c(seq(1,nAgents-1,1))){ #loop through different ratios of social learners
    indLearners <- nAgents - socLearners #number of individual learners
    group <- c(rep('Ind', indLearners), rep('Soc', socLearners)) #group makeup
    for (r in 1:rounds){ #loop through rounds
      set.seed(nrow(socialSimDF)) #set the seed for each new game
      Qmat <- matrix(Q0,  nrow = nAgents, ncol = k) #reset Q-values; this variable is now a matrix with one row per agent (unused for social learners)
      socialObs <- rep(NA, nAgents) #social observations
      for (t in 1:trials){ #loop through trials
        for (agent in 1:nAgents){ #loop through agents
          if (group[agent] == 'Ind'){
            #Individual learners
            p <- softmax(beta, Qmat[agent,]) #compute softmax policy; we assume for now that all individual agents have the same parameter combo as before
            action <- sample(1:k,size = 1, prob=p) #sample action
            reward <- banditGenerator(action) #generate reward
            Qmat[agent,action] <- Qmat[agent,action] + alpha*(reward - Qmat[agent,action]) #update q-values
            chosen <- rep(0, k) #create an index for the chosen option
            chosen[action]<- 1 #1 = chosen, 0 not
            socialObs[agent] <- action
            #save data
            trialDF <- data.frame(trial = t, agent = agent, type = group[agent], ratio = socLearners, round = r, reward = reward)
            socialSimDF <- rbind(socialSimDF,trialDF)
          }else if (group[agent] == 'Soc'){ 
            #social learning agent
            if (t==1){ #first trial is random, since there are no social observations
              p = rep(1/k, k) #uniform probabilities
            }else{
              socialFreq <- table(factor(socialObs[-agent], levels = 1:k)) + .0001 #Frequency of each action + a very small number to avoid divide by zero
              p <- socialFreq^theta #compute probabilities
              p <- p/sum(p) 
            }
            action <- sample(1:k,size = 1, prob=p) #sample action according to policy
            reward <- banditGenerator(action) #generate reward
            chosen <- rep(0, k) #create an index for the chosen option
            chosen[action]<- 1 #1 = chosen, 0 not
            socialObs[agent] <- action
            #save data
            trialDF <- data.frame(trial = t, agent = agent, type = group[agent], ratio = socLearners, round = r, reward = reward)
            socialSimDF <- rbind(socialSimDF,trialDF)
          }
        }
      }
    }
  }
  return(socialSimDF)
}

#socialsimDF <- simSocialData(nAgents = 10, rounds = 100, trials = 25) #commented out because this is very slow. Load it from precomputed data instead
socialsimDF <- readRDS('data/socialSimsTutorial2.Rds')

perfDF <- socialsimDF  %>% group_by(type,  ratio) %>% summarize(performance = mean(reward))
popDF <- socialsimDF  %>% group_by(ratio) %>% summarize(performance = mean(reward))
popDF$type = 'Pop'

combinedDF <- rbind(perfDF,popDF )
combinedDF$type <- factor(combinedDF$type, levels = c('Ind', 'Soc', 'Pop'))
#Plot results
ggplot(subset(combinedDF, ratio>0 & ratio<8),  aes(x = ratio/8, y = performance, color = type))+
  geom_line(aes(linetype=type))+
  theme_classic()+
  labs( x =  "% Social Learners", y = "Avg. Performance")+
  scale_color_manual(values = c("#d7191c","#2b83ba",  "black"), name = "")+
  scale_linetype_manual(values = c("dashed", "dotted", "solid"), name = "")+
  ylab('Average Rewards')+
  scale_x_continuous(labels=scales::percent)+
  ggtitle("Rogers' Paradox")+
  theme(legend.position = c(0.1,0.1), legend.justification = c(0,0), legend.background=element_blank())