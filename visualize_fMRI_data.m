% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_2.7';
roi_dir = fullfile(main_project_dir, 'roiMasks');

% Paths to the ROI masks
roi_mask_path1 = fullfile(roi_dir, 'Left_Hippocampus-maxprob-thr50-2mm.nii');
roi_mask_path2 = fullfile(roi_dir, 'Right_Hippocampus-maxprob-thr50-2mm.nii');

% Load the ROI masks
V_roi1 = spm_vol(roi_mask_path1);
roi_mask1 = spm_read_vols(V_roi1);

V_roi2 = spm_vol(roi_mask_path2);
roi_mask2 = spm_read_vols(V_roi2);

% Combine the ROI masks (logical OR operation)
combined_roi_mask = (roi_mask1) | (roi_mask);

% Save the combined ROI mask to a new file
V_combined_roi = V_roi1; % Copy the header information from one of the ROI masks
V_combined_roi.fname = fullfile(roi_dir, 'Combined_Hippocampus_ROI.nii');
spm_write_vol(V_combined_roi, combined_roi_mask);

% Path to the sample contrast image
sample_con_file = fullfile(main_project_dir, 'Subj_001', 'Count_Upreg_fDisp', 'con_0001.nii');
V_sample = spm_vol(sample_con_file);

% Reslice the combined ROI mask to the space of the sample contrast image
spm_reslice({V_sample.fname, V_combined_roi.fname}, struct('which', 1, 'mean', false));

% Load the resliced combined ROI mask
resliced_roi_mask_path = fullfile(roi_dir, 'rCombined_Hippocampus_ROI.nii');
V_resliced_roi = spm_vol(resliced_roi_mask_path);
resliced_combined_roi_mask = spm_read_vols(V_resliced_roi);

% Ensure resliced combined ROI mask is non-empty
fprintf('Number of non-zero elements in resliced combined ROI mask: %d\n', nnz(resliced_combined_roi_mask));

% Subject identifiers for each group
F_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007', 'Subj_009', 'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044'};
control_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057'};

% Initialize cell arrays to store the paths to contrast images
NF_contrasts = {};
control_contrasts = {};

% Collect the contrast images for NF group
for i = 1:length(NF_subjects)
    for j = 1:8 % con_0001.nii to con_0008.nii
        con_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            NF_contrasts{end+1, 1} = con_file;
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Collect the contrast images for control group
for i = 1:length(control_subjects)
    for j = 1:8 % con_0001.nii to con_0008.nii
        con_file = fullfile(main_project_dir, control_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            control_contrasts{end+1, 1} = con_file;
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Verify the number of contrast images gathered for each group
fprintf('Number of contrast images gathered for NF group: %d\n', length(NF_contrasts));
fprintf('Number of contrast images gathered for Control group: %d\n', length(control_contrasts));

% Initialize the arrays for storing beta weights
num_conditions = 8;
NF_betas = zeros(length(NF_subjects), num_conditions);
control_betas = zeros(length(control_subjects), num_conditions);

% Extract beta weights for the NF group
for i = 1:length(NF_subjects)
    for j = 1:num_conditions
        con_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            V = spm_vol(con_file);
            Y = spm_read_vols(V);
            % Ensure the dimensions match
            if all(size(Y) == size(resliced_combined_roi_mask))
                % Extract beta weights using the resliced combined ROI mask
                NF_betas(i, j) = mean(Y(resliced_combined_roi_mask > 0)); % Mean beta value within the resliced combined ROI
                % Print the extracted beta value
                fprintf('NF subject %s, condition %d: beta value = %f\n', NF_subjects{i}, j, NF_betas(i, j));
            else
                fprintf('Dimension mismatch for file: %s\n', con_file);
            end
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Extract beta weights for the control group
for i = 1:length(control_subjects)
    for j = 1:num_conditions
        con_file = fullfile(main_project_dir, control_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            V = spm_vol(con_file);
            Y = spm_read_vols(V);
            % Ensure the dimensions match
            if all(size(Y) == size(resliced_combined_roi_mask))
                % Extract beta weights using the resliced combined ROI mask
                control_betas(i, j) = mean(Y(resliced_combined_roi_mask > 0)); % Mean beta value within the resliced combined ROI
                % Print the extracted beta value
                fprintf('Control subject %s, condition %d: beta value = %f\n', control_subjects{i}, j, control_betas(i, j));
            else
                fprintf('Dimension mismatch for file: %s\n', con_file);
            end
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Print the extracted beta values for debugging
fprintf('Extracted beta values for NF group:\n');
disp(NF_betas);

fprintf('Extracted beta values for control group:\n');
disp(control_betas);

% Compute means and standard errors
mean_NF = mean(NF_betas, 'omitnan');
se_NF = std(NF_betas, 'omitnan') / sqrt(size(NF_betas, 1));

mean_control = mean(control_betas, 'omitnan');
se_control = std(control_betas, 'omitnan') / sqrt(size(control_betas, 1));

% Print the means and standard errors for debugging
fprintf('Mean beta values for NF group:\n');
disp(mean_NF);

fprintf('Standard error for NF group:\n');
disp(se_NF);

fprintf('Mean beta values for control group:\n');
disp(mean_control);

fprintf('Standard error for control group:\n');
disp(se_control);

% Create a bar plot with error bars
figure;

% Number of conditions
x = 1:num_conditions;

% Plot NF data
bar(x - 0.2, mean_NF, 0.4, 'FaceColor', 'b');
hold on;
% Plot control data
bar(x + 0.2, mean_control, 0.4, 'FaceColor', 'r');

% Add error bars
errorbar(x - 0.2, mean_NF, se_NF, 'k', 'LineStyle', 'none');
errorbar(x + 0.2, mean_control, se_control, 'k', 'LineStyle', 'none');

% Customize the plot
set(gca, 'XTick', x);
set(gca, 'XTickLabel', {'con001', 'con002', 'con003', 'con004', 'con005', 'con006', 'con007', 'con008'});
xlabel('Conditions');
ylabel('Beta Weights');
legend({'NF (Neurofeedback)', 'Control'});
title('Comparison of Beta Weights in the Hippocampus Across Conditions');
hold off;
