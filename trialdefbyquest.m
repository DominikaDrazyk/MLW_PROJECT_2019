function [trl, event] = trialdefbyquest(cfg)

% read  header and events info
hdr   = ft_read_header(cfg.dataset);
event = ft_read_event(cfg.dataset);

% search for triggers
value  = [event(find(strcmp('Stimulus', {event.type}))).value];
sample = [event(find(strcmp('Stimulus', {event.type}))).sample];

% number of samples before and after the trigger
pretrig  = -round(cfg.trialdef.pre  * hdr.Fs);
posttrig =  round(cfg.trialdef.post * hdr.Fs);
trl = [];

% classification terms, answers from triggers
answers_list = [];
for k = 1:length(cfg.ans_table)                   
    answers_list = [answers_list str2double(cfg.ans_table(k))]; % adds n answers to tle list
end

value_array = regexp(value, '[S  ]', 'split');    % splits value list with 'S ' 
value_array(strncmpi(value_array,'',1)) = [];     % remove empty elements
value_array = str2double(value_array);            % convert string into numbers

for i = 1:length(value_array)-1
    if value_array(i) == 1 && value_array(i+1) ~= 1         % for the last '1' in a list 
        trlbegin = sample(i);                               % def trial beginning
        n = 1;
        t = 0;
        
        while value_array(i+n) > 10 && t == 0     % search for next smaller than '10' (it is 1st question)
            n = n+1;
            
        question_list = [value_array(i+n)];
        for j = 1:length(cfg.ans_table)-1         
            question_list = [question_list value_array(i+n+j)]; % adds n questions to tle list
        end

        good = 0;
        for o = 1:length(question_list)
            if answers_list(o) == 0 || answers_list(o) ~= 0 && question_list(o) == answers_list(o)
                good = good + 1; % gives a point for every correct answer or if question was invalid 
            end
        end
        
        if good == length(cfg.ans_table)              % if all answers are correct (or question was invalid) it saves trial
            trlend   = sample(i+n+2);                 % def trial end    
            offset   = pretrig;
            newtrl   = [trlbegin trlend offset];
            trl      = [trl; newtrl];
            t = 1;
        end

        end
    end
end
end
