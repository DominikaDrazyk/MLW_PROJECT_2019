function [] = selectbyquest(trials, EEG_name,EEG)
s = size(trials); % number of defined trials

if s(1) > 0
    
    if s(1) > 1 % first 2
    EEG1 = pop_select( EEG,'point',[trials(1,1) trials(1,2)]);
    EEG2 = pop_select( EEG,'point',[trials(2,1) trials(2,2)]);
    F_EEG = pop_mergeset(EEG1, EEG2); 
    
    if s(1) > 2 % every next above 2
         for m = 1:(s(1)-2)
             EEG3 = pop_select( EEG,'point',[trials(m+2,1) trials(m+2,2)] );
             F_EEG = pop_mergeset(F_EEG, EEG3);
         end
    end
    
    else % when only one 
        F_EEG = pop_select( EEG,'point',[trials(1,1) trials(1,2)] );
    end   
    
    F_EEG = eeg_checkset( F_EEG );
    pop_eegplot( F_EEG, 1, 1, 1); % visual check in popup window

    fname = [pwd,'\modif_',num2str(size(trials,1)) ,'_', EEG_name]; % adds number of selected trials to the name of new files
    pop_writebva(F_EEG,fname);                                      % creates new modif*eeg modif*vmrk modif*vhdr file of selected trials  
    pop_writeeeg(F_EEG,fname);
    %pop_writeeeg(F_EEG,fname, 'TYPE', 'EEG');
    
else % when none
    disp('no trials to select');
end