# MRExperiment
Project for the mental rotation test. Contains both code for conducting the experiment as well as code for reading and analyzing the data.

## Conducting the Experiment
The experiment is programmed according to Jost and Jansen, 2019. The runtime of the experiment is controlled by time instead of the number of stimuli, although an adaptation should be easily possible by adding break conditions.
### Presentation
This folder contains the Presentation software code for the experiment. Development will probably not continue, as OpenSesame provides a free and open source alternative.
### OpenSesame
This folder contains the OpenSesame software code for the experiment. Timing calculations for feedback and fixations try to take into account calculation and preparation times. In first tests, they are presented for ~60ms longer than specified (although the start time is calculated on the basis of the stimulus start time and the reaction time, not the actual time at which the feedback/fixation is presented, i.e. we try to control the pause between reaction and presentation of the next stimulus, not the time that the fixation/feedback is actually shown).
### Changes
The random order was changed such that all stimuli will be shown once (in random order), before they are shown again once (in random order). Overlap conditions characterize the minimal number of stimuli at start and end of these random lists, that are not the same. E.g. no stimulus occurs in both the last 20 of one random list and the first 20 of the next random list. While this guarantess a minimal distance between reoccurences of the same stimulus, this is not a uniform distance condition between stimulus occurences. E.g. the 20th to last stimulus of one random list cannot occur again in the next 39 stimuli, while both the 21st to last and the last stimulus cannot occur again in the next 20 stimuli. As reoccurence distance is random and not uniform anyway, this should not pose a problem.

#### OpenSesame
The same random stimulus order is used for all blocks (practice and main), such that the next block continues after the last stimulus of the previous block.

## R Code
R code is in all other files. Data folder contains some example data, so the code can be executed.

## Literature 
Jost, L., & Jansen, P. (2019). A novel approach to analyzing mirrored stimuli in chronometric mental rotation and description of a flexible extended library of stimuli. Unpublished manuscript.
