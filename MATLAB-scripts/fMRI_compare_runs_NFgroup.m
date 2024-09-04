% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Subject identifiers for the NF group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007',  'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};

% Initialize arrays to hold contrast values for both runs
NF_contrast_values_Run1 = zeros(length(NF_subjects), 1);
NF_contrast_values_Run7 = zeros(length(NF_subjects), 1);

% Collect contrast values for Run 1 and Run 7 for the NF group
for i = 1:length(NF_subjects)
    % Paths to the contrast images for the first and seventh runs
    con1_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', 'con_0001.nii');
    con7_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', 'con_0007.nii');
    
    if isfile(con1_file) && isfile(con7_file)
        % Load the contrast images
        V1 = spm_vol(con1_file);
        Y1 = spm_read_vols(V1);
        V7 = spm_vol(con7_file);
        Y7 = spm_read_vols(V7);
        
        % Ensure the mask dimensions match the images
        if all(size(Y1) == size(resliced_combined_roi_mask)) && all(size(Y7) == size(resliced_combined_roi_mask))
            % Extract the contrast values within the hippocampus mask
            Y1_masked = Y1(resliced_combined_roi_mask > 0);
            Y7_masked = Y7(resliced_combined_roi_mask > 0);
            
            % Calculate mean contrast values within the hippocampal ROI
            NF_contrast_values_Run1(i) = mean(Y1_masked, 'omitnan');
            NF_contrast_values_Run7(i) = mean(Y7_masked, 'omitnan');
        else
            fprintf('Dimension mismatch for subject %s\n', NF_subjects{i});
        end
    else
        if ~isfile(con1_file)
            fprintf('File not found: %s\n', con1_file);
        end
        if ~isfile(con7_file)
            fprintf('File not found: %s\n', con7_file);
        end
    end
end

% Perform a paired t-test comparing Run 1 and Run 7 within the NF group
[h, p, ci, stats] = ttest(NF_contrast_values_Run1, NF_contrast_values_Run7);

% Display the results
fprintf('Paired t-test results (Run 7 vs Run 1):\n');
fprintf('t-value: %.3f\n', stats.tstat);
fprintf('p-value: %.3f\n', p);
fprintf('Confidence Interval: [%.3f, %.3f]\n', ci);
fprintf('Mean contrast value (Run 1): %.3f\n', mean(NF_contrast_values_Run1));
fprintf('Mean contrast value (Run 7): %.3f\n', mean(NF_contrast_values_Run7));

% Calculate Cohen's d for the paired comparison
mean_diff = mean(NF_contrast_values_Run7 - NF_contrast_values_Run1);
pooled_std = std(NF_contrast_values_Run7 - NF_contrast_values_Run1);
cohen_d = mean_diff / pooled_std;

fprintf('Cohen''s d: %.3f\n', cohen_d);

% Plot the mean contrast values with error bars for Run 1 and Run 7
figure;
scatter(ones(length(NF_contrast_values_Run1), 1), NF_contrast_values_Run1, 'filled', 'r'); % Scatter points for Run 1
hold on;
scatter(2*ones(length(NF_contrast_values_Run7), 1), NF_contrast_values_Run7, 'filled', 'b'); % Scatter points for Run 7
errorbar(1, mean(NF_contrast_values_Run1), std(NF_contrast_values_Run1)/sqrt(length(NF_contrast_values_Run1)), 'k', 'LineWidth', 2); % Error bar for Run 1
errorbar(2, mean(NF_contrast_values_Run7), std(NF_contrast_values_Run7)/sqrt(length(NF_contrast_values_Run7)), 'k', 'LineWidth', 2); % Error bar for Run 7
plot(1, mean(NF_contrast_values_Run1), 'b*', 'MarkerSize', 10, 'LineWidth', 2); % Mean marker for Run 1
plot(2, mean(NF_contrast_values_Run7), 'r*', 'MarkerSize', 10, 'LineWidth', 2); % Mean marker for Run 7

% Customize the plot
ylabel('Contrast Value in Hippocampus');
title('Hippocampus Activation: Run 1 vs Run 7 (Paired T-Test)');
xlim([0.5, 2.5]);
xticks([1 2]);
xticklabels({'Run 1', 'Run 7'});

hold off;


% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Subject identifiers for the NF group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007',  'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};

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
