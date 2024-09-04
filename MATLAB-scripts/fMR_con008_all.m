% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Define the output directory for storing the second-level analysis
second_level_dir = fullfile(main_project_dir, 'Second_Level_Analysis_con_0008_all');
if ~exist(second_level_dir, 'dir')
    mkdir(second_level_dir);
end

% Subject identifiers for all participants
all_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007', 'Subj_009', 'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077', 'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071','Subj_077' 'Subj_080', 'Subj_081'};

% Initialize cell array to store the paths to all contrast images
con8_paths = {};

% Collect the contrast images for all participants
for i = 1:length(all_subjects)
    con8_file = fullfile(main_project_dir, all_subjects{i}, 'Count_Upreg_fDisp', 'con_0008.nii');
    if isfile(con8_file)
        con8_paths{end+1, 1} = con8_file;
    else
        fprintf('File not found: %s\n', con8_file);
    end
end

% Verify the number of contrast images gathered
fprintf('Number of contrast images gathered for con_0008: %d\n', length(con8_paths));

% Initialize SPM
spm('defaults', 'FMRI');
spm_jobman('initcfg');

% Specify the one-sample t-test model
matlabbatch = {};
matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir};
matlabbatch{1}.spm.stats.factorial_design.des.t1.scans = con8_paths;
matlabbatch{1}.spm.stats.factorial_design.des.t1.gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.t1.ancova = 0;

% Additional options (e.g., masking, global normalization)
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

% Model estimation
matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir, 'SPM.mat')};

% Save the batch job
save(fullfile(second_level_dir, 'batch_one_sample_ttest.mat'), 'matlabbatch');

% Run the batch job
spm_jobman('run', matlabbatch);

fprintf('Second-level model estimation completed and saved to: %s\n', second_level_dir);
