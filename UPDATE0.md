Fix grammer, make following plan more clear and coherent:

You are taking over a Haskell project. 

0. Start with reading README.md

1. Then read all the Haskell sources and build a solid understanding of project implementation.

2. Then read public/js and public/css files to understand the frontend.

3. Make sure you have a solid understanding of project. Ask if anything is unclear to you.

4. We are going to make lifespan of every chibik finite. To do that make following changes to project core, DSL, rules, frontend and docs.

4.0 Let every chibik have two individual value: number of turn chibik has existed and probability to dia at the start of next move

4.1 This probability should increase every turn. The specific law of change is up to you but The expected lifespan of free chibik (that is not engaged in killing or reprocducing) should be controlled, this pamaper as well as starting probability at birth should be specified in the *.rule *depending on species* (that is important). So you should do some math and simulations to understand what law of probability chage with turns would look like . Write your findings to SIMS.md  

4.2 Killing should be non-deterministic now. The probability of successful killing is dependent on species and is contained in *.rules 

4.3 The successful killing leads to decrease of probability of to die at the next turn. Law is up to you, it global is, same for all species, its parameter should be specified as enviroment parameter. Use your research results from 4.1.

4.5 Reproduction is also non-deterministic now. For every species there should be a distribution of number of offspring (including zero). It should be speciefied in the *.rules as a list of probabilitites. 

4.6 Successful reproduction (giving birth to non-zero number of descendants) leads to increase of probability to die at the next turn. Law is up to you, it is global, same for all species, its parameter should be specified as enviroment parameter. Use your research results from 4.1.

4.7 Track every chibik turns for life. Make a histogram over ages for every species and life-display then in the UI

4.8 There is a rules/triplet.rule for two basic species and one hybrid. It is useful as minimal model.

4.9 Remeber to update both rules in rules folder and LANGUAGE.md descrition docs.

4.10 Make a fast tuning of all new and old parameters for interesting game simulations (we want to see some kind of oscillations). But don't try too hard on that one, just rough guesses. See 4.8

4.11 At the end double check all the docs. They should represent current state of affairs. Escpecailly rules files and DSL description.

4.12 Commit often


