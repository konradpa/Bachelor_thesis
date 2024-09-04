% Define the output directories for storing the second-level analysis for each group
second_level_dir_NF = fullfile(main_project_dir, 'Second_Level_Analysis_con_0008_NF');
second_level_dir_Sham = fullfile(main_project_dir, 'Second_Level_Analysis_con_0008_Sham');

% Create the directories if they don't exist
if ~exist(second_level_dir_NF, 'dir')
    mkdir(second_level_dir_NF);
end
if ~exist(second_level_dir_Sham, 'dir')
    mkdir(second_level_dir_Sham);
end

% Subject identifiers for each group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007', 'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};
sham_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071', 'Subj_080', 'Subj_081'};

% Initialize cell arrays to store the paths to all contrast images for each group
con8_paths_NF = {};
con8_paths_Sham = {};

% Collect the contrast images for the NF group
for i = 1:length(NF_subjects)
    con8_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', 'con_0008.nii');
    if isfile(con8_file)
        con8_paths_NF{end+1, 1} = con8_file;
    else
        fprintf('File not found: %s\n', con8_file);
    end
end

% Collect the contrast images for the Sham group
for i = 1:length(sham_subjects)
    con8_file = fullfile(main_project_dir, sham_subjects{i}, 'Count_Upreg_fDisp', 'con_0008.nii');
    if isfile(con8_file)
        con8_paths_Sham{end+1, 1} = con8_file;
    else
        fprintf('File not found: %s\n', con8_file);
    end
end

% Verify the number of contrast images gathered
fprintf('Number of contrast images gathered for NF group (con_0008): %d\n', length(con8_paths_NF));
fprintf('Number of contrast images gathered for Sham group (con_0008): %d\n', length(con8_paths_Sham));

% Initialize SPM
spm('defaults', 'FMRI');
spm_jobman('initcfg');

% Specify the one-sample t-test model for NF group
matlabbatch = {};
matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir_NF};
matlabbatch{1}.spm.stats.factorial_design.des.t1.scans = con8_paths_NF;
matlabbatch{1}.spm.stats.factorial_design.des.t1.gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.t1.ancova = 0;

% Additional options (e.g., masking, global normalization) for NF group
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

% Model estimation for NF group
matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir_NF, 'SPM.mat')};

% Save the batch job for NF group
save(fullfile(second_level_dir_NF, 'batch_one_sample_ttest_NF.mat'), 'matlabbatch');

% Run the batch job for NF group
spm_jobman('run', matlabbatch);

fprintf('Second-level model estimation for NF group completed and saved to: %s\n', second_level_dir_NF);

% Specify the one-sample t-test model for Sham group
matlabbatch = {};
matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir_Sham};
matlabbatch{1}.spm.stats.factorial_design.des.t1.scans = con8_paths_Sham;
matlabbatch{1}.spm.stats.factorial_design.des.t1.gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.t1.ancova = 0;

% Additional options (e.g., masking, global normalization) for Sham group
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

% Model estimation for Sham group
matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir_Sham, 'SPM.mat')};

% Save the batch job for Sham group
save(fullfile(second_level_dir_Sham, 'batch_one_sample_ttest_Sham.mat'), 'matlabbatch');

% Run the batch job for Sham group
spm_jobman('run', matlabbatch);

fprintf('Second-level model estimation for Sham group completed and saved to: %s\n', second_level_dir_Sham);
