% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Subject identifiers for the NF group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007', 'Subj_009', 'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};

% Initialize cell arrays to store the paths to the contrast images
con1_paths = {};
con7_paths = {};

% Collect the contrast images for the NF group
for i = 1:length(NF_subjects)
    % Paths to the contrast images for the first and seventh runs
    con1_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', 'con_0001.nii');
    con7_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', 'con_0007.nii');
    
    if isfile(con1_file) && isfile(con7_file)
        con1_paths{end+1, 1} = con1_file;
        con7_paths{end+1, 1} = con7_file;
    else
        if ~isfile(con1_file)
            fprintf('File not found: %s\n', con1_file);
        end
       if ~isfile(con7_file)
            fprintf('File not found: %s\n', con7_file);
        end
    end
end

% Verify the number of contrast images gathered
fprintf('Number of contrast images for Run 1: %d\n', length(con1_paths));
fprintf('Number of contrast images for Run 7: %d\n', length(con7_paths));

% Ensure the paths are column cell arrays of strings
con1_paths = reshape(con1_paths, [], 1);
con7_paths = reshape(con7_paths, [], 1);

% Define the path for the second-level analysis directory for the paired t-test
second_level_dir = fullfile(main_project_dir, '2ndLevel_PairedTTest_Run1_vs_Run7');
if ~exist(second_level_dir, 'dir')
    mkdir(second_level_dir);
end

% Initialize SPM
spm('defaults', 'FMRI');
spm_jobman('initcfg');

% Prepare pairs for the paired t-test
pairs = cell(length(con1_paths), 1);
for i = 1:length(con1_paths)
    pairs{i, 1} = {con1_paths{i}; con7_paths{i}};
end

% Ensure pairs are cell arrays of cell arrays
pairs = reshape(pairs, [], 1);

% Specify the paired t-test model
matlabbatch = {};
matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir};
matlabbatch{1}.spm.stats.factorial_design.des.pt.pair = struct('scans', pairs);
matlabbatch{1}.spm.stats.factorial_design.des.pt.gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.pt.ancova = 0;

% Additional options (e.g., masking, global normalization)
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

% Model estimation
matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir, 'SPM.mat')};

% Define the contrast for Run 7 > Run 1
matlabbatch{3}.spm.stats.con.spmmat = {fullfile(second_level_dir, 'SPM.mat')};
matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'Run 7 > Run 1';
matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [-1 1];
matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';

% Save the batch job
save(fullfile(second_level_dir, 'batch_paired_ttest.mat'), 'matlabbatch');

% Run the batch job
spm_jobman('run', matlabbatch);
