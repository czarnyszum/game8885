**Project Handover and Implementation Plan**

You are taking over a Haskell project. Follow this structured plan to get up to speed and implement finite lifespans for chibiks.

**Initial Onboarding (0–3)**
0. Start by reading the `../README.md` (и `AGENT-INIT.md` в этой папке) to get a high-level overview of the project.
1. Thoroughly read all Haskell source files to build a solid understanding of the core implementation.
2. Examine the `public/js` and `public/css` files to understand the frontend architecture.
3. Ensure you have a complete grasp of the entire project. Ask questions about anything that remains unclear before proceeding.

**Core Implementation: Finite Lifespans (4.0–4.12)**
The primary goal is to make every chibik's lifespan finite. This requires coordinated changes across the core logic, DSL, rules files, frontend, and documentation.

4.0. **Add lifespan attributes**: Give each chibik two individual values: (a) the number of turns it has existed, and (b) its probability of dying at the start of the next move. Add this to the end of the simulation cycle.

4.1. **Define the aging law**: This probability must increase every turn. The exact growth law is up to you, but the *expected lifespan* of a free chibik (one not engaged in killing or reproducing) must be controllable. The starting probability at birth and the lifespan-controlling parameters must be specified in the `*.rules` file **per species** (this is crucial). Perform mathematical analysis and simulations to select a suitable law of probability change over turns, and document your findings in `SIMS.md`.

4.2. **Make killing non-deterministic**: The probability of a successful kill must depend on the species and be defined in the `*.rules` file.

4.3. **Apply kill penalties/rewards**: A successful kill must *decrease* the chibik's probability of dying on the next turn. The specific law for this decrease is up to you, but it must be global (identical for all species). Its parameter must be defined as an environment parameter, using insights from your research in step 4.1.

4.4. **Make reproduction non-deterministic**: For each species, define a probability distribution over the number of offspring (including zero). This must be specified in the `*.rules` file as a list of probabilities.

4.5. **Apply reproduction penalties**: A successful reproduction event (producing a non-zero number of offspring) must *increase* the chibik's probability of dying on the next turn. The specific law for this increase is up to you, but it must be global (identical for all species). Its parameter must be defined as an environment parameter, using insights from your research in step 4.1.

4.6. **Implement lifespan tracking and UI**: Track the total number of turns each chibik lives (its age at death). Generate a histogram of lifespans for each species and display these histograms in the UI. Update at every turn.

4.7. **Use the minimal model**: The `rules/triplet.rule` file defines two basic species and one hybrid. Use it as a useful minimal model for testing.

4.8. **Update rules and documentation**: Remember to update both the rule files in the `rules/` folder and the DSL description in `LANGUAGE.md` to reflect the new parameters.

4.9. **Tune parameters for interesting dynamics**: Perform a quick tuning of all new and existing parameters to achieve interesting game simulations (ideally, some kind of population oscillation). Don't spend too much time on this—rough estimates are sufficient. Start from the `triplet.rule` model (see step 4.7).

4.10. **Final documentation review**: Double-check all documentation to ensure it accurately represents the current state of the project, especially the rules files and the DSL description.

4.11. **Commit often**: Make frequent, atomic commits throughout the entire process.


