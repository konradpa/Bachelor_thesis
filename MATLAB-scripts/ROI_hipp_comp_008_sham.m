% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';
roi_dir = fullfile(main_project_dir, 'roiMasks');

% Path to the already combined and resliced ROI mask
resliced_roi_mask_path = fullfile(roi_dir, 'rCombined_Hippocampus_ROI.nii');
V_resliced_roi = spm_vol(resliced_roi_mask_path);
resliced_combined_roi_mask = spm_read_vols(V_resliced_roi);

% Ensure the resliced combined ROI mask is non-empty
fprintf('Number of non-zero elements in resliced combined ROI mask: %d\n', nnz(resliced_combined_roi_mask));

% Subject identifiers for all participants in the sham group
sham_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071', 'Subj_080', 'Subj_081'};

% Initialize the array for storing contrast values for the sham group
sham_contrast_values = zeros(length(sham_subjects), 1);

% Extract contrast values for the sham group
for i = 1:length(sham_subjects)
    con_file_upregulation = fullfile(main_project_dir, sham_subjects{i}, 'Count_Upreg_fDisp', 'con_0008.nii');  % Upregulation > Counting contrast file

    if isfile(con_file_upregulation)
        % Load the contrast image
        V_upregulation = spm_vol(con_file_upregulation);
        Y_upregulation = spm_read_vols(V_upregulation);
        
        % Ensure the dimensions match
        if all(size(Y_upregulation) == size(resliced_combined_roi_mask))
            % Extract contrast values using the resliced combined ROI mask
            contrast_values_upregulation = Y_upregulation(resliced_combined_roi_mask > 0);
            
            % Calculate the mean contrast value within the ROI, omitting NaNs
            sham_contrast_values(i) = mean(contrast_values_upregulation, 'omitnan');
            
            % Print the extracted contrast values for debugging
            fprintf('Sham subject %s: Upregulation > Counting contrast value = %f\n', sham_subjects{i}, sham_contrast_values(i));
        else
            fprintf('Dimension mismatch for subject: %s\n', sham_subjects{i});
        end
    else
        fprintf('File not found for subject: %s\n', sham_subjects{i});
    end
end

% Perform one-sample t-test against 0
[h, p, ci, stats] = ttest(sham_contrast_values);

% Display the results
fprintf('One-sample t-test results for sham group:\n');
fprintf('t-value: %.3f\n', stats.tstat);
fprintf('p-value: %.3f\n', p);
fprintf('Confidence Interval: [%.3f, %.3f]\n', ci);
fprintf('Mean contrast value (Upregulation > Counting): %.3f\n', mean(sham_contrast_values));

% Create a scatter plot showing the mean with error bars for the sham group
figure;
scatter(ones(length(sham_contrast_values), 1), sham_contrast_values, 'filled', 'r'); % Scatter points
hold on;
mean_value = mean(sham_contrast_values);
ci_range = [ci(1) - mean_value, ci(2) - mean_value]; % Calculate CI range
errorbar(1, mean_value, ci_range(1), ci_range(2), 'k', 'LineWidth', 2); % Error bar for CI
plot(1, mean_value, 'b*', 'MarkerSize', 10, 'LineWidth', 2); % Mean marker

% Customize the plot
ylabel('Contrast Value in Hippocampus');
title('Hippocampus Activation: Mean and Confidence Interval with Data Points for Sham Group');
xlim([0.9, 1.1]);

% Calculate Cohen's d
std_dev = std(sham_contrast_values);
cohen_d = mean_value / std_dev;
fprintf('Cohen''s d: %.3f\n', cohen_d);

% Number of subjects
n = length(sham_contrast_values);

% Compute the standard error of Cohen's d
se_d = sqrt((1/n) + (cohen_d^2 / (2*(n-1))));

% Calculate the confidence interval (95%)
% Z value for 95% confidence is approximately 1.96
ci_d_lower = cohen_d - 1.96 * se_d;
ci_d_upper = cohen_d + 1.96 * se_d;

fprintf('Cohen''s d Confidence Interval: [%.3f, %.3f]\n', ci_d_lower, ci_d_upper);
