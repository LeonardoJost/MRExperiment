# MRExperiment
Project for the mental rotation test. Contains both code for conducting the experiment as well as code for reading and analyzing the data in R.

## Conducting the Experiment
The experiment is programmed according to Jost and Jansen, 2019. The runtime of the experiment is controlled by time instead of the number of stimuli, although an adaptation should be easily possible by adding break conditions. For all controlling software, stimuli and system parameters have to be set for usage.
### Presentation
This folder contains the Presentation software (Neurobehavioral Systems, Inc., Berkeley, CA, www.neurobs.com) code for the experiment. Development will probably not continue, as open source alternatives such as OpenSesame are available.
#### Usage
Questionaire and instructions are read from txt files. Design parameters and foldernames of stimuli are entered in line 15-37 and 211-235 in the sce file, some fixed instructions can be changed in line 486-505.
### OpenSesame
This folder contains the OpenSesame software (Mathôt, Schreij, & Theeuwes, 2012; https://osdoc.cogsci.nl/) code for the experiment. 
#### Usage
All stimuli and txt files containing instructions and questions should be loaded into the experiment. Parameters can be changed in the inline script parameters. Blocks can be modified in the loop Blocks.

Questions should be entered the following way: (Type)(Options)(ID)(condition). Type should be either: Multiple (for multiple choice), TextInput (for custom text input) or ShowText (default, only ok button is presented as answer). If type is Multiple, options should include all possible answers separated by ',' (no spaces), for other Types no options should be entered. ID is optional, default is the number of the question. Conditions can be entered if some questions should only be presented depending on previous answers. At the moment only 'ID==answer' is supported and the question is only presented if the question with 'ID' is anwered with 'answer'.

The blocks are by default controlled by time and the number of stimuli. If either all stimuli are processed or time is over, the experiment should finish. The transition between blocks is only controlled by time and has to be changed by hand, if necessary.
#### notes
Xpyriment is used as backend because psycho causes errors when entering non-ASCII characters in the questionaire. In general all backends should work.

Timing calculations for feedback and fixations try to take into account calculation and preparation times. In first tests, they are presented for ~60ms longer than specified, which is closer than specifying the time directly into the sketchpads for feedback and fixations (~110ms in that case) (although the start time is calculated on the basis of the stimulus start time and the reaction time, not the actual time at which the feedback/fixation is presented, i.e. we try to control the pause between reaction and presentation of the next stimulus, not the time that the fixation/feedback is actually shown). Shifting the calculation parts around did not yield different results as of now. Timing differences seem in part influenced by display refresh frequency and loading of the stimulus images into the sketchpad.

The questionaire repeats questions that are not answered (as there is an option to skip questions available). As infinite loops are hard to program, the maximum number of questions including reasked questions is set to 1000 (cycles in QuestionsLoop). While this should suffice for most cases, this can easily be changed. (this could probably also be done better in inline scripts by repeating the form inside the OpenSesame loop)

The naming of the stimulus files is coded in the inline script SetNamesAndResponses.

ID and block are named aaID and aaBlock so they always appear first in the logfiles.
### Changes compared to previous versions
The random order was changed such that all stimuli will be shown once (in random order), before they are shown again once (in random order) and so on. Overlap conditions characterize the minimal number of stimuli at start and end of these random lists, that are not the same. E.g. no stimulus occurs in both the last 20 of one random list and the first 20 of the next random list. While this guarantess a minimal distance between reoccurences of the same stimulus, this is not a uniform distance condition between stimulus occurences. E.g. the 20th to last stimulus of one random list cannot occur again in the next 39 stimuli, while both the 21st to last and the last stimulus cannot occur again in the next 20 stimuli. As reoccurence distance is random and not uniform anyway, this should not pose a problem.

#### OpenSesame
The same random stimulus order is used for all blocks (practice and main), such that the next block continues after the last stimulus of the previous block.

## R Code
R code is in all other files. Data folder contains some example data, so the code can be executed.

## Literature 
Jost, L., & Jansen, P. (2019). A novel approach to analyzing mirrored stimuli in chronometric mental rotation and description of a flexible extended library of stimuli. Unpublished manuscript.

Mathôt, S., Schreij, D., & Theeuwes, J. (2012). OpenSesame: An open-source, graphical experiment builder for the social sciences. Behavior Research Methods, 44(2), 314-324. doi:10.3758/s13428-011-0168-7