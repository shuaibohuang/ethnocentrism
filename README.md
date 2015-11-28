# ethnocentrism
Examining the Stages in the Evolution of Ethnocentrism with Kin Selection Tendency
Shuaibo Huang
McGill University

Abstract
Previous computer simulations have suggested that ethnocentric strategy is favored by evolution. However, these simulations did not take into account the tendency of kin selection, which refers to the theory that animals tend to look out for their close relatives, so that genes will survive and be passed on in future generations. We added extension onto current computational models to simulate the evolution of ethnocentrism with kin selection tendency. Our simulations have shown that ethnocentrism is the dominant strategy throughout our simulations, except in one case. Different from previous modellings, our simulations yielded the result that selfish agents tend to win over traitors and humanitarians. In addition, our simulations also have suggested that kin selection does help our gene to survive longer and be passed on in future generations.
Keywords: ethnocentrism; evolution; kin selection

Examining the Stages in the Evolution of Ethnocentrism with Kin Selection Tendency
Previous computer simulations have examined the theory that humans have evolved a tendency to cooperate with members of their own ethic group but discriminate against members of other ethnic groups (Hammond & Axelrod, 2006). Such a tendency is defined as ethnocentrism. These simulations provided a possible explanation for the mystery of why people cooperate so much in the infamous Prisoner’s Dilemma game. In addition, further study has shown that an early stage in which humanitarians, who cooperate with all other agents, may dominate (Shultz, Hartshorn & Hammond, 2008).
However, these simulations did not include the effect of kin selection. Kin selection, also known as inclusive fitness, is the evolutionary tendency for animals to protect their close relatives, so that their genes will survive and be passed on in future generations. 
Mockingbirds have been observed to be more likely to feed hungry nestlings that are more closely related to them than other hungry nestlings (Curry, 1988). Among humans, genetic relatedness influences cooperation as well. In a puzzle task that required cooperation, identical twins, who share all their genes, were found to cooperate about twice as often as fraternal twins, who share only half of their genes (Segal, 1984). In addition, a study has shown that when hypothetical situations are described to participants, they report being more willing to help closely related individuals than strangers (Nitschke et al, 2004).
Thus, from the perspective of kin selection, agents should always cooperate with their children, parents, and close relatives.  We added extension onto previous algorithms to simulate the effect of kin selection and re-examine the evolutionary theory on ethnocentrism and other cooperation strategies. 


Methods

The Artificial World 

Agents Extension was made onto the agent-based Hammond-Axelrod algorithm. Each agent in our simulations has five identity tags: an ID (a unique number), a tag denoting their membership in one of four different groups, an in-group strategy (cooperate or defect), an out-group strategy (cooperate or defect), and a parent tag. The parent tag of an agent is the ID number of its parent. For example, if Agent One (ID: 001) reproduced Agent Two, Agent Two’s parent tag would be 001. Each agent also has a reproductive potential and initialized as 0.12.

Strategies Four possible strategies could evolve in our artificial world; selfish agents always defect; traitors cooperate only with agents of other groups; ethnocentric agents cooperate only with agents of their groups; and finally a humanitarian always cooperates with anyone of any group. Besides following these four strategies, all agents cooperate with their siblings, parents, and children to simulate kin selection. Kin selection was not generalized to grandparents or agents who share the same ancestors, because studies have shown that kin selection only applies to close relatives (Segal, 1984).  
Evolutionary Cycles We define an evolutionary cycle as the process for our artificial world to go through a cycle of four phases: immigration, interaction, reproduction, and death. 

During the immigration phase, a new agent is created under an immigration rate with randomly selected strategic and group tags. Immigrants are their own parents, since they are created without receiving genotypes from other agents. 
In the interaction phase, all agents interact with their neighbors. If Agent A decides to cooperate with Agent B, Agent A’s reproductive potential is reduced by 0.01 and B’s reproductive potential is increased by 0.03. Whereas if agent A chooses to defect, its reproductive potential is decreased by 0.03 and B’s reproductive potential is reduced by 0.01. 
During the reproduction phase, all agents are selected in a random order to reproduce a clone under their reproductive potential. A clone has the same strategic and group tags as its parent. However, strategic tags could mutate during the reproductive phase. The clone is placed in a randomly selected empty neighboring location if any exist. 
Finally, in the death phase, each agent has a chance of being removed from the artificial world. The default death rate is 0.10.

Procedure
Five simulations were run with 1000 evolutionary cycles each. Numbers of agents with each of the four strategies were recorded at the end of each cycle.

Results
Mean strategy frequencies across the five simulations over 1000 cycles each are shown in Figure one, and distributions of agents in one simulation are shown in Figure two. Number of agents with each of the four strategies become stable around the 400th cycles. Ethnocentric strategy dominated in all five worlds with an average proportion of 57.8%. 

Discussion
Ethnocentric strategy dominates in all of our simulations, except in one case. Such a result suggests that the humanitarian strategies competing successfully with ethnocentric ones in early stage was largely a matter of chance. 
Compared to previous simulations without kin selection (Shultz, Hartshorn & Hammond, 2008), our model yields a less significant dominance of the ethnocentric strategy at the end of 1000 evolutionary cycles (57.8% compares to 75%). In other words, other genotypes preserved more agents than in simulations without kin selection. Our artificial world is more diverse. Therefore, our simulations also suggest that kin selection helps our genes to survive longer and be passed to future generations. Such a finding consists with the empirical hypothesis that kin selection promotes genotype survival.
Humanitarians, traitors, and selfish agents always competed early in evolution, but selfish individuals win eventually over the other two kinds of agents. Such a result should be predictable, since selfish agents with kin-selection tendency are similar to ethnocentric agents in the way they choose to cooperate or not. 
	Due to the nature of computational modelling, our simulation precisely controlled many variables, which mostly cannot be done in research with human subjects and real societies. However, limitations exist. Real societies are far more complex than our artificial world. Many other factors than we discussed above affect one’s decision on whom to cooperate with. For instance, people are more readily to cooperate and give resources to an integration partner whom they know to have a good reputation (Wedekind&Milinski, 2000).
	Further studies should be done by adding more extensions onto our current models to gain a deeper understanding on the evolution of cooperative strategies. For example, studies have shown that strangers are significantly more likely to be cooperative in rural communities than in urban areas (Steblay, 1987), so agents in a small cluster should be more cooperative than ones in larger clusters. 
References

Hammond, R. A., & Axelrod, R. (2006). The evolution of ethnocentrism. Journal of Conflict Resolution, 50(6), 926-936.

Shultz, T. R., Hartshorn, M., & Hammond, R. A. (2008). Stages in the evolution of ethnocentrism. In Proceedings of the 30th annual conference of the cognitive science society (pp. 1244-1249).

Curry, R. L. (1988). Influence of kinship on helping behavior in Galapagos mockingbirds. Behavioral Ecology and Sociobiology, 22(2), 141-152.

Segal, N. L. (1984). Cooperation, competition, and altruism within twin sets: A reappraisal. Ethology and Sociobiology, 5(3), 163-177.

Nitschke, J. B., Nelson, E. E., Rusch, B. D., Fox, A. S., Oakes, T. R., & Davidson, R. J. (2004). Orbitofrontal cortex tracks positive mood in mothers viewing pictures of their newborn infants. Neuroimage, 21(2), 583-592.

Wedekind, C., & Milinski, M. (2000). Cooperation through image scoring in humans. Science, 288(5467), 850-852.

Steblay, N. M. (1987). Helping behavior in rural and urban environments: A meta-analysis. Psychological Bulletin, 102(3), 346.

