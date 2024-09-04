% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Load the resliced combined ROI mask
resliced_roi_mask_path = fullfile(roi_dir, 'rCombined_Hippocampus_ROI.nii');
V_resliced_roi = spm_vol(resliced_roi_mask_path);
resliced_combined_roi_mask = spm_read_vols(V_resliced_roi);

% Ensure resliced combined ROI mask is non-empty
fprintf('Number of non-zero elements in resliced combined ROI mask: %d\n', nnz(resliced_combined_roi_mask));

% Subject identifiers for each group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007',  'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};
control_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071', 'Subj_080', 'Subj_081'};

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

% Initialize the arrays for storing contrast values
num_conditions = 8;
NF_contrast_values = zeros(length(NF_subjects), num_conditions);
control_contrast_values = zeros(length(control_subjects), num_conditions);

% Extract contrast values for the NF group
for i = 1:length(NF_subjects)
    for j = 1:num_conditions
        con_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            V = spm_vol(con_file);
            Y = spm_read_vols(V);
            % Ensure the dimensions match
            if all(size(Y) == size(resliced_combined_roi_mask))
                % Extract contrast values using the resliced combined ROI mask
                contrast_values = Y(resliced_combined_roi_mask > 0);
                if ~all(isnan(contrast_values))
                    NF_contrast_values(i, j) = mean(contrast_values, 'omitnan'); % Mean contrast value within the resliced combined ROI
                else
                    fprintf('All NaN values for NF subject %s, condition %d\n', NF_subjects{i}, j);
                end
                % Print the extracted contrast value
                fprintf('NF subject %s, condition %d: contrast value = %f\n', NF_subjects{i}, j, NF_contrast_values(i, j));
            else
                fprintf('Dimension mismatch for file: %s\n', con_file);
            end
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Extract contrast values for the control group
for i = 1:length(control_subjects)
    for j = 1:num_conditions
        con_file = fullfile(main_project_dir, control_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            V = spm_vol(con_file);
            Y = spm_read_vols(V);
            % Ensure the dimensions match
            if all(size(Y) == size(resliced_combined_roi_mask))
                % Extract contrast values using the resliced combined ROI mask
                contrast_values = Y(resliced_combined_roi_mask > 0);
                if ~all(isnan(contrast_values))
                    control_contrast_values(i, j) = mean(contrast_values, 'omitnan'); % Mean contrast value within the resliced combined ROI
                else
                    fprintf('All NaN values for Control subject %s, condition %d\n', control_subjects{i}, j);
                end
                % Print the extracted contrast value
                fprintf('Control subject %s, condition %d: contrast value = %f\n', control_subjects{i}, j, control_contrast_values(i, j));
            else
                fprintf('Dimension mismatch for file: %s\n', con_file);
            end
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Print the extracted contrast values for debugging
fprintf('Extracted contrast values for NF group:\n');
disp(NF_contrast_values);

fprintf('Extracted contrast values for control group:\n');
disp(control_contrast_values);

% Compute means and standard errors
mean_NF = mean(NF_contrast_values, 'omitnan');
se_NF = std(NF_contrast_values, 'omitnan') / sqrt(size(NF_contrast_values, 1));

mean_control = mean(control_contrast_values, 'omitnan');
se_control = std(control_contrast_values, 'omitnan') / sqrt(size(control_contrast_values, 1));

% Print the means and standard errors for debugging
fprintf('Mean contrast values for NF group:\n');
disp(mean_NF);

fprintf('Standard error for NF group:\n');
disp(se_NF);

fprintf('Mean contrast values for control group:\n');
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
xlabel('Contrasts ');
ylabel('Mean Contrast Value in Hippocampus');
legend({'NF (Neurofeedback)', 'Control'});
title('Comparison of Contrast Values in the Hippocampus Across Conditions');
hold off;