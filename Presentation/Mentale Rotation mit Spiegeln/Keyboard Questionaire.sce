#     Copyright (C) 2019  Leonardo Jost
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#header parameters
response_matching = simple_matching;
default_font_size = 24;
active_buttons=0;

event_code_delimiter=";";
stimulus_properties=block,string,color,string;

begin;

picture {
	text { 
		caption= " ";
		max_text_width=1024;
	}question;
	x=0; top_y=360;#768/2=384, -font_size
	text {
		caption=" ";
		max_text_width=1024;
	} inputText;
	x=0; y=-150;
} inputPic;



begin_pcl;

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

#program parameters
array<string> questions[]=readStringArrayFromFile("Fragebogen.txt");
string gender="";
string inputString;
loop int i=1 until i>questions.count()
begin
	if(questions[i].find("(Gender)")>0) then		
		questions[i]=questions[i].replace("(Gender)","");
		question.set_caption(questions[i],true);
		gender=system_keyboard.get_input(inputPic,inputText);
	elseif(questions[i].find("(Gender==w)")>0 && !(gender.find("w")>0 || gender.find("W")>0 )) then
		#do not show question
		logfile.add_event_entry("not female");
	else
		questions[i]=questions[i].replace("(Gender==w)","");
		question.set_caption(questions[i],true);
		inputString=system_keyboard.get_input(inputPic,inputText);
	end;
	if(i==1 || !(inputString=="")) then
		i=i+1;
		logfile.add_event_entry("q"+string(i));
	end;
end;
loop int i=1 until i>1
begin
	if(inputString=="ende") then
		i=i+1;
	else
		inputString=system_keyboard.get_input(inputPic,inputText);
	end;
end;

