% enter space separated answers for questions in correct order, if a question is unvalid, enter '0' 
prompt = {'Enter proper answear or 0 if question unvalid, eg. 3 0 0'};
title = 'ANSWERS';
dims = [1 35];
answer = inputdlg(prompt,title,dims);

answer = regexp(answer, '\s+', 'split');
answer = vertcat(answer{:});

% load a data file
currEEG = pop_loadbv(pwd, 'txcn_07.vhdr');
currEEG.setname='txcn_07.vhdr';

cfg                         = [];
cfg.dataset                 = currEEG.setname;
cfg.trialfun                = 'trialdefbyquest';
cfg.trialdef.pre            = 0.1;
cfg.trialdef.post           = 0.1;
cfg.ans_table               = answer;      % proper answear or 0 if question unvalid, eg. [3,0,6]

cfg                         = ft_definetrial(cfg);
data                        = ft_preprocessing(cfg);

t = cfg.trl;
eeg_name = currEEG.setname;

selectbyquest(t,eeg_name,currEEG);         % creates new modif*eeg modif*vmrk modif*vhdr file of selected trials  

% error 1 : cannt save to EEG format (biosig toolbox error)
% selectbyquest: line 27-28 

