% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Subject identifiers for each group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007',  'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};
control_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071', 'Subj_080', 'Subj_081'};

% Loop over each condition (con_0001 to con_0008)
for condition = 1:8
    % Initialize cell arrays to store the paths to all contrast images for the current condition
    NF_contrasts = {};
    control_contrasts = {};

    % Collect the contrast images for NF group for the current condition
    for i = 1:length(NF_subjects)
        con_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', condition));
        if isfile(con_file)
            NF_contrasts{end+1, 1} = con_file;
        else
            fprintf('File not found: %s\n', con_file);
        end
    end

    % Collect the contrast images for control group for the current condition
    for i = 1:length(control_subjects)
        con_file = fullfile(main_project_dir, control_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', condition));
        if isfile(con_file)
            control_contrasts{end+1, 1} = con_file;
        else
            fprintf('File not found: %s\n', con_file);
        end
    end

    % Verify the number of contrast images gathered for each group
    fprintf('Number of contrast images gathered for NF group (condition %d): %d\n', condition, length(NF_contrasts));
    fprintf('Number of contrast images gathered for Control group (condition %d): %d\n', condition, length(control_contrasts));

    % Define the path for the second-level analysis directory for the current condition
    second_level_dir = fullfile(main_project_dir, sprintf('2ndLevel_GroupComparison_con%02d', condition));
    if ~exist(second_level_dir, 'dir')
        mkdir(second_level_dir);
    end

    % Initialize SPM
    spm('defaults', 'FMRI');
    spm_jobman('initcfg');

    % Specify the two-sample t-test model
    matlabbatch = {};
    matlabbatch{1}.spm.stats.factorial_design.dir = {second_level_dir};
    matlabbatch{1}.spm.stats.factorial_design.des.t2.scans1 = NF_contrasts;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.scans2 = control_contrasts;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.dept = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.variance = 1;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.gmsca = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.ancova = 0;

    % Additional options (e.g., masking, global normalization)
    matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
    matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

    % Model estimation
    matlabbatch{2}.spm.stats.fmri_est.spmmat = {fullfile(second_level_dir, 'SPM.mat')};

     % Specify the contrast 
    matlabbatch{3}.spm.stats.con.spmmat = {fullfile(second_level_dir, 'SPM.mat')};
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'NF > Control';
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [1 -1];
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';


    % Save the batch job
    save(fullfile(second_level_dir, 'batch_group_comparison.mat'), 'matlabbatch');

    % Run the batch job
    spm_jobman('run', matlabbatch);
end
