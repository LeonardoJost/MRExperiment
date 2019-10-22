#header parameters
response_matching = simple_matching;
default_font_size = 48;
default_background_color=0,0,0;  #black
default_text_color=255,255,255;  #white
#default_background_color=255,255,255;
#default_text_color=0,0,0;
active_buttons=2;

event_code_delimiter=";";
stimulus_properties=block,string,model,string,mirror,string,angle,string;

begin;

$numberOfModels=16;
$numberOfAngles=7; #45 90 135 180 225 270 315  (0 degrees included by default)
$angleDiff=45;
$folder="black_back\\";
$folderY="$folder\y\\";
$folderZ="$folder\z\\";
$w=400; #width of images
$h=400; #height of images
$fixationSize=40;
		
#array with different original pictures
array{
	LOOP $i $numberOfModels;
		$k = '$i+1';
		bitmap {
			filename = "$folderY$k\_y_0_a.png";
			width=$w;height=$h;
			description = "Modell$k\A";
		}"orig$k\A";
	ENDLOOP;
} originalsA;

#array with different original pictures
array{
	LOOP $i $numberOfModels;
		$k = '$i+1';
		bitmap {
			filename = "$folderY$k\_y_0_b.png";
			width=$w;height=$h;
			description = "Modell$k\B";
		}"orig$k\B";
	ENDLOOP;
} originalsB;

#arrays with pictures per model
#loop over models
LOOP $i $numberOfModels;
$k = '$i+1';
	array{
		#original image, rotation 0 deg
		bitmap {
			filename = "$folderY$k\_y_0_a.png";
			width=$w;height=$h;
			description = "originalY0";
		};
		#mirrored image, rotation 0 deg
		bitmap {
			filename = "$folderY$k\_y_0_b.png";
			width=$w;height=$h;
			description = "mirroredY0";
		};
	LOOP $j $numberOfAngles;
		$deg = '($j+1)*$angleDiff';
		#original image, rotation around Y axis
		bitmap {
			filename = "$folderY$k\_y_$deg\_a.png";
			width=$w;height=$h;
			description = "originalY$deg";
		};
		#original image, rotation around Z axis
		bitmap {
			filename = "$folderZ$k\_z_$deg\_a.png";
			width=$w;height=$h;
			description = "originalZ$deg";
		};
		#mirrored image, rotation around Y axis
		bitmap {
			filename = "$folderY$k\_y_$deg\_b.png";
			width=$w;height=$h;
			description = "mirroredY$deg";
		};
		#mirrored image, rotation around Z axis
		bitmap {
			filename = "$folderZ$k\_z_$deg\_b.png";
			width=$w;height=$h;
			description = "mirroredZ$deg";
		};
	ENDLOOP;
	}"modell$k";
ENDLOOP;

#picture of stimulus
picture {
	bitmap orig1A;
	x=-300; y=150;
	bitmap orig1A;
	x=300;y=150;
	bitmap orig1A;
	x=0;y=-150;
	text{caption="";}timeText; x=0; y=300;
} stim;

#picture for fixation
picture {
	text { caption = "+";
		font_size = $fixationSize;
		};
	x = 0; y = 0;
	text timeText; x=0;y=300;
} fixation;

#main trial
trial {
	#wait for user response (only in stimulus phase)
	trial_duration = forever;
	trial_type=first_response;
	all_responses=false;
	
	#fixation
	#stimulus_event {
	#	picture fixation;
	#	time = 0;
	#};
	#stimulus
	stimulus_event {
		picture stim;
		time = 0;
		code = "stim";
	} stim_event;
} main_trial;

#show trial
trial {
	#wait for user response (only in stimulus phase)
	trial_duration = 200;
	trial_type=first_response;
	
	#stimulus
	stimulus_event {
		picture stim;
		time = 0;
		code = "demo";
	} show_event;
} show_trial;

#feedback presentation
trial {
	trial_duration=1000;
	#feedback text
	picture {
		text{caption="";font_size=40;} feedback_text;
		x=0;y=0;
		text timeText; x=0;y=300;
	};
} feedback;

#countdown presentation
trial {
	trial_duration=1;
	#feedback text
	picture {
		text{caption="Nächste Stufe in";font_size=40;} countdown_time;
		x=0;y=0;
		text{caption="Nächste Stufe in";font_size=40;} countdown_text; x=0;y=300;
	};
} countdown;

#left aligned Text (used for introductory Text)
trial {
	#wait for user response
	trial_duration = forever;
	trial_type=first_response;
	stimulus_event {
		picture {
		text{caption="";
			font_size=24;
			text_align=align_left;
			formatted_text=true;
		} leftText;
		x=0;y=0;
		#text timeText; x=0;y=300;
	};
	time = 0;
	};
} leftTextTrial;

#centered Text (used for Pause after Feedback and ending Text)
trial {
	#wait for user response
	trial_duration = forever;
	trial_type=first_response;
	stimulus_event {
		picture {
		text{caption="";font_size=24;} centerText;
		x=0;y=0;
		#text timeText; x=0;y=300;
	};
	time = 0;
	};
} centerTextTrial;

#questionaire
picture {
	text { 
		caption= " ";
		max_text_width=1024;
		font_size=24;
		formatted_text=true;
	}question;
	x=0; top_y=360;#768/2=384, -font_size
	text {
		caption=" ";
		max_text_width=1024;
		font_size=24;
	} inputText;
	x=0; y=-150;
} inputPic;


begin_pcl;
#parameters
#array positions
int MODEL=1;
int ANGLE=2;
#initialize array of pictures
array<bitmap> models[0][0];
models.add(modell1);
models.add(modell2);
models.add(modell3);
models.add(modell4);
models.add(modell5);
models.add(modell6);
models.add(modell7);
models.add(modell8);
models.add(modell9);
models.add(modell10);
models.add(modell11);
models.add(modell12);
models.add(modell13);
models.add(modell14);
models.add(modell15);
models.add(modell16);
#feedback texts
string falsch="\u2717";
string richtig="\u2713";

##subroutine definition

#get time
sub string getTime(int startTime)
begin
	int time=clock.time()-startTime;
	int min = time /1000/60;
	time=time-min*1000*60;
	int sec = time/1000/15 *15;
	string mins="";
	if min<10 then
		mins="0";
	end;
	mins=mins+string(min);
	string secs="";
	if sec<10 then
		secs="0";
	end;
	secs=secs+string(sec);
	return mins+":"+secs;
end;


#create randomized trial order
#add all possibilities in shuffled order, repeat for number of repeats
#if all are supposed to be shuffled, shuffle again before return
sub array<int,2> createTrials(int repeats, string filename, int maxOk)
begin
	array<int> trialslist[0][0];
	array<int> trialslistTemp[0][0];
	loop int j=1 until j>modell1.count()
	begin
		loop int k=1 until k>originalsA.count()
		begin
			array<int> temp[2];
			temp[MODEL]=k;
			temp[ANGLE]=j;
			trialslistTemp.add(temp);
			k=k+1;
		end;
		j=j+1;
	end;
	loop int i=1 until i>repeats
	begin	
		i=i+1;
		loop int j=1 until j>1
		begin
			trialslistTemp.shuffle();
			j=2;
			if trialslist.count()>maxOk then 
				loop int k=1 until k>maxOk
				begin
					loop int m=1 until m>maxOk
					begin
						if trialslistTemp[k]==trialslist[trialslist.count()+1-m]
						then
							j=1;
							#term.print_line("e"+string(k)+","+string(m)+","+string(maxOk));
						end;
						m=m+1;
					end;
					k=k+1;
				end;
			end;
		end;
		loop int j=1 until j>trialslistTemp.count()
		begin
			trialslist.add(trialslistTemp[j]);
			j=j+1;
		end;
	end;
	#print list to file
	#output_file out = new output_file;
	#out.open(filename+".txt");
	#loop
	#	int i=1
	#until
	#	i>repeats*modell1.count()*originalsA.count()
	#begin
	#	out.print(string(trialslist[i][1])+"\t"+string(trialslist[i][2])+"\n");
	#	i=i+1;
	#end;
	#out.close();
	return trialslist;
end;

#read trialslist from txt file
sub array<int,2> readTrialsFromFile(string filename)
begin
	array<int> trialslist[0][0];
	input_file in = new input_file;
	in.open(filename);
	loop until in.end_of_file() || !in.last_succeeded()
	begin
		array<int> temp[2];
		temp[MODEL]=in.get_int();
		temp[ANGLE]=in.get_int();
		if (temp[MODEL]!=0) then  #last line reads 0
			trialslist.add(temp);
		end;
	end;	
	return trialslist;
end;

#present trials
sub int present_trials(array<int,2> trials_list, int iStart, bool showfeedback, string type_code, int timeLimit, int startOverhead)
begin
	int startTime=clock.time()+startOverhead; #if overhead<0 set startTime in the past
	feedback_text.set_caption("+",true);
	int i=iStart;
	loop until i>trials_list.count() || clock.time()-startTime>timeLimit
	begin
		#get randomized data
		int model_index=trials_list[i][MODEL];
		bitmap nextOrigLeft=originalsA[model_index];
		bitmap nextOrigRight=originalsB[model_index];
		int angle_index=trials_list[i][ANGLE];
		bitmap nextAngle=models[model_index][angle_index];
		#randomize left and right originals
		array<bool> switchLeftRightOriginals[]={true,false};
		switchLeftRightOriginals.shuffle();
		if (switchLeftRightOriginals[1]) then 
			nextOrigLeft=originalsB[model_index];
			nextOrigRight=originalsA[model_index];
		end;
		#set description
		stim_event.set_event_code(type_code+";"+nextOrigLeft.description()+";"+nextAngle.description()+";"+string(angle_index));
		#set target button (right response)
		if ((nextAngle.description().find("orig")>0 && !switchLeftRightOriginals[1])||(nextAngle.description().find("mirror")>0 && switchLeftRightOriginals[1])) then
			stim_event.set_target_button(1);
		else
			stim_event.set_target_button(2);
		end;
		
		#timeText.set_caption("modell: "+nextOrig.description()+" Angle: " + nextAngle.description(),true);
		timeText.set_caption(getTime(startTime),true);
		#set stimulus
		stim.set_part(1,nextOrigLeft);
		stim.set_part(2,nextOrigRight);
		stim.set_part(3,nextAngle);
		#show trial
		main_trial.present();
		#feedback
		if (showfeedback) then
			stimulus_data last=stimulus_manager.last_stimulus_data();
			string feedcap=falsch;
			if (last.type() == stimulus_data::HIT) then
				feedcap = richtig;
			end;
			timeText.set_caption(getTime(startTime),true);
			feedback_text.set_caption(feedcap,true);
		end;
		#show feedback (or fixation if no feedback)
		feedback.present();
		i=i+1;
	end;
	return i;
end;

#show demonstration trials
sub bool showTrials(int model, bool mirror, bool zAxis)
begin
	int mirr=0; string mirrString="";
	if (mirror) then mirr=1; mirrString="(gespiegelt)"; end;
	int z=0; string axis="x";
	if (zAxis) then z=1; axis="z"; end;
	timeText.set_caption("Demonstration Rotation um die " + axis + "-Achse\n" + mirrString,true);
	loop int i=1 until i>8
	begin
		int j=4*i-5+z+2*mirr;
		if (j<3) then
			j=1+mirr;
		end;
		bitmap orig=models[model][1];
		bitmap angle=models[model][j];
		#set stimulus
		stim.set_part(1,orig);
		stim.set_part(2,angle);
		show_event.set_target_button(2);
		#show trial
		show_trial.present();
		i=i+1;
		if (i>8) then
			i=1;
		end;
		stimulus_data last=stimulus_manager.last_stimulus_data();
		if (last.type() == stimulus_data::HIT) then
			return true;
		end;
		if (last.type() == stimulus_data::INCORRECT) then
			return false;
		end;
		#if (last.type() != stimulus_data::MISS) then
		#	return true;
		#end;
	end;
	return false;
end;

sub showLeftTextTrial(string in)
begin
	leftText.set_caption(in,true);
	leftTextTrial.present();
end;

sub showCenterTextTrial(string in)
begin
	centerText.set_caption(in,true);
	centerTextTrial.present();
end;

sub string replacePlaceholders(string orig, string f1, string f2)
begin
	orig=orig.replace("F1",f1);
	orig=orig.replace("F2",f2);
	return orig;
end;

#read String from txt file
sub string readStringFromFile(string filename)
begin
	string read="";
	input_file in = new input_file;
	in.open(filename);
	#in.set_delimiter('\n');
	loop until in.end_of_file() || !in.last_succeeded()
	begin
		read.append(in.get_line()+"\n");
	end;	
	return read;
end;

#read String from txt file
sub array<string,1> readStringArrayFromFile(string filename)
begin
	array<string> read[0];
	input_file in = new input_file;
	in.open(filename);
	in.set_delimiter('\n');
	loop until in.end_of_file() || !in.last_succeeded()
	begin
		read.add(in.get_line().replace("--","\n"));
	end;	
	return read;
end;

sub int showQuestions(array<string,1> questions, string type_code)
begin
	int startTime=clock.time();
	string inputString;
	loop int i=1 until i>questions.count()
	begin
		if(questions[i].find("(Abbruch)")>0) then		
			questions[i]=questions[i].replace("(Abbruch)","");
			logfile.add_event_entry(type_code+";q"+string(i));
			question.set_caption(questions[i],true);
			inputString=system_keyboard.get_input(inputPic,inputText);
			if (inputString=="10") then
				return -1;
			end;
		else
			logfile.add_event_entry(type_code+";q"+string(i));
			question.set_caption(questions[i],true);
			inputString=system_keyboard.get_input(inputPic,inputText);
		end;
		i=i+1;
	end;
	return clock.time()-startTime;
end;

sub showCountdown(int duration)
begin
	int startTime=clock.time();
	loop until (clock.time()-startTime>duration)
	begin
		int time=duration-(clock.time()-startTime);
		int min = time /1000/60;
		time=time-min*1000*60;
		int sec = time/1000+1; #ceil
		countdown_time.set_caption(string(sec),true);
		countdown.present();
	end;
end;


##main program
#parameters
int durationPractice=10;
int durationMain=30;
#load trialslists
#array<int> trialslistPractice[][]=createTrials(10,"",10);
array<int> trialslistMain[][]=createTrials(10,"",10);

/*int oldCode=0;
if (oldCode==1) then
## Practice Trials
#show instructions
showLeftTextTrial(replacePlaceholders(readStringFromFile("InstructionsPractice.txt"),richtig,falsch));
#present Trials
present_trials(trialslistPractice,true,"practice",durationPractice*60*1000);
#End
#showCenterTextTrial("Ende der Übungseinheit\nDrücken Sie eine Maustaste, wenn sie bereit zum fortfahren sind.");
## Main Trials
feedback.set_duration(500);
#show instructions
showLeftTextTrial(readStringFromFile("Instructions.txt"));
#present Trials
present_trials(trialslistMain,false,"main",durationMain*60*1000);
#End
#showCenterTextTrial("Ende des Experiments\n");startTime
end;*/

int durationSeconds=45;
int durationTotal=60;
int practiceTrials=3;
#load questions
array<string> questions[]=readStringArrayFromFile("FragebogenStufentest.txt");
#practice instructions
showLeftTextTrial(replacePlaceholders(readStringFromFile("InstructionsPractice.txt"),richtig,falsch));
#practice trials
int passedTrials=0;
int overhead=0;
int trialIndex=1;
loop until passedTrials==practiceTrials
begin
	passedTrials=passedTrials+1;
	int startTime=clock.time()+overhead; #if overhead<0 set startTime in the past
	#present Trials
	trialIndex=present_trials(trialslistMain,trialIndex,true,"practice"+string(passedTrials),durationSeconds*1000,overhead);
	showQuestions(questions,"questionairePractice"+string(passedTrials));
	if (passedTrials!=practiceTrials) then
		#break until finalTime
		if (durationTotal*1000-(clock.time()-startTime)>0) then
			showCountdown(durationTotal*1000-(clock.time()-startTime));
			overhead=0;
		else
			overhead=durationTotal*1000-(clock.time()-startTime);
		end;
	end;
end;
## Main Trials
feedback.set_duration(500);
#show instructions
showLeftTextTrial(readStringFromFile("Instructions.txt"));
#present trials
passedTrials=0;
overhead=0;
int endCondition=0;
loop until endCondition==-1
begin
	passedTrials=passedTrials+1;
	int startTime=clock.time()+overhead;
	#present Trials
	trialIndex=present_trials(trialslistMain,trialIndex,false,"main"+string(passedTrials),durationSeconds*1000,overhead);
	endCondition=showQuestions(questions,"questionaireMain"+string(passedTrials));
	if (endCondition!=-1) then
		#break until finalTime
		if (durationTotal*1000-(clock.time()-startTime)>0) then
			showCountdown(durationTotal*1000-(clock.time()-startTime));
			overhead=0;
		else
			overhead=durationTotal*1000-(clock.time()-startTime);
		end;
	end;
end

