% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_2.7';

% List all items in the main project directory to verify access
main_dir_contents = dir(main_project_dir);
fprintf('Contents of %s:\n', main_project_dir);
for i = 1:length(main_dir_contents)
    fprintf('  %s (isdir: %d)\n', main_dir_contents(i).name, main_dir_contents(i).isdir);
end

% Initialize a cell array to store paths to contrast images
contrast_images = {};

% Attempt to list all subject directories matching the pattern 'Subj_*'
subject_dirs = dir(fullfile(main_project_dir, 'Subj_*'));
fprintf('Number of subject directories found: %d\n', length(subject_dirs));

% Debugging: List names of found directories
for i = 1:length(subject_dirs)
    fprintf('Found subject directory: %s\n', subject_dirs(i).name);
end

% Loop through each subject directory if any are found
for i = 1:length(subject_dirs)
    % Construct the full path to the subject's Count_Upreg_fDisp directory
    subject_dir = fullfile(subject_dirs(i).folder, subject_dirs(i).name, 'Count_Upreg_fDisp');
    
    % Check if the subject directory exists
    if isfolder(subject_dir)
        % List files in the subject directory to verify contents
        subject_files = dir(subject_dir);
        fprintf('Files in %s:\n', subject_dir);
        for k = 1:length(subject_files)
            fprintf('  %s\n', subject_files(k).name);
        end
        
        % Loop through each required contrast image (con001 to con008)
        for j = 1:8 % con001 to con008
            con_file = fullfile(subject_dir, sprintf('con_%04d.nii', j));
            
            % Check if the contrast file exists
            if isfile(con_file)
                contrast_images{end+1, 1} = con_file; % Add to cell array
            else
                fprintf('File not found: %s\n', con_file);
            end
        end
    else
        fprintf('Directory not found: %s\n', subject_dir);
    end
end

% Display the gathered contrast images (for verification)
disp(contrast_images);


% Verify the number of contrast images gathered
fprintf('Number of contrast images gathered: %d\n', length(contrast_images));

% Define the path for the second-level analysis directory
second_level_dir = fullfile(main_project_dir, '2ndLevel_upregulation_counting');
mkdir(second_level_dir);

% Initialize SPM
spm('defaults', 'FMRI');
spm_jobman('initcfg');

% Specify 2nd-level model
matlabbatch = {};

% Design specification
matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir};
matlabbatch{1}.spm.stats.factorial_design.des.t1.scans = contrast_images;
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

% Model estimation
matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir, 'SPM.mat')};

% Save the batch job
save(fullfile(second_level_dir, 'batch_2nd_level.mat'), 'matlabbatch');



