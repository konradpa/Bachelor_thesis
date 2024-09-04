% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';

% Load the resliced combined ROI mask
resliced_roi_mask_path = fullfile(roi_dir, 'rCombined_Hippocampus_ROI.nii');
V_resliced_roi = spm_vol(resliced_roi_mask_path);
resliced_combined_roi_mask = spm_read_vols(V_resliced_roi);

% Ensure resliced combined ROI mask is non-empty
fprintf('Number of non-zero elements in resliced combined ROI mask: %d\n', nnz(resliced_combined_roi_mask));

% Subject identifiers for each group
NF_subjects = {'Subj_001', 'Subj_002', 'Subj_003', 'Subj_007', 'Subj_011', 'Subj_012', 'Subj_014', 'Subj_015', 'Subj_016', 'Subj_019', 'Subj_020', 'Subj_028', 'Subj_033', 'Subj_041', 'Subj_043', 'Subj_044', 'Subj_058', 'Subj_059', 'Subj_061', 'Subj_062', 'Subj_063', 'Subj_064', 'Subj_066', 'Subj_077'};
control_subjects = {'Subj_025', 'Subj_026', 'Subj_027', 'Subj_030', 'Subj_034', 'Subj_037', 'Subj_038', 'Subj_039', 'Subj_045', 'Subj_046', 'Subj_051', 'Subj_054', 'Subj_056', 'Subj_057', 'Subj_071', 'Subj_080', 'Subj_081'};

% Initialize the arrays for storing contrast values
num_conditions = 7; % Adjust based on the number of contrasts you have
NF_contrast_values = zeros(length(NF_subjects), num_conditions);
control_contrast_values = zeros(length(control_subjects), num_conditions);

% Extract contrast values for the NF group
for i = 1:length(NF_subjects)
    for j = 1:num_conditions
        con_file = fullfile(main_project_dir, NF_subjects{i}, 'Count_Upreg_fDisp', sprintf('con_%04d.nii', j));
        if isfile(con_file)
            V = spm_vol(con_file);
            Y = spm_read_vols(V);
            if all(size(Y) == size(resliced_combined_roi_mask))
                contrast_values = Y(resliced_combined_roi_mask > 0);
                if ~all(isnan(contrast_values))
                    NF_contrast_values(i, j) = mean(contrast_values, 'omitnan'); % Mean contrast value within the resliced combined ROI
                else
                    fprintf('All NaN values for NF subject %s, condition %d\n', NF_subjects{i}, j);
                end
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
            if all(size(Y) == size(resliced_combined_roi_mask))
                contrast_values = Y(resliced_combined_roi_mask > 0);
                if ~all(isnan(contrast_values))
                    control_contrast_values(i, j) = mean(contrast_values, 'omitnan'); % Mean contrast value within the resliced combined ROI
                else
                    fprintf('All NaN values for Control subject %s, condition %d\n', control_subjects{i}, j);
                end
            else
                fprintf('Dimension mismatch for file: %s\n', con_file);
            end
        else
            fprintf('File not found: %s\n', con_file);
        end
    end
end

% Calculate the average contrast values across subjects for each group
mean_NF_contrast_values = mean(NF_contrast_values, 1);
mean_control_contrast_values = mean(control_contrast_values, 1);

% Perform linear regression for the NF group using the average contrast values
X = (1:num_conditions)'; % Independent variable (condition indices)
Y_NF = mean_NF_contrast_values'; % Dependent variable (average contrast values)
lm_NF = fitlm(X, Y_NF);
fprintf('Linear Regression Results for NF Group:\n');
disp(lm_NF);

% Perform linear regression for the Control group using the average contrast values
Y_control = mean_control_contrast_values'; % Dependent variable (average contrast values)
lm_control = fitlm(X, Y_control);
fprintf('Linear Regression Results for Control Group:\n');
disp(lm_control);

% Visualize the linear regression for both groups
figure;
hold on;

% Plot NF group
plot(X, Y_NF, 'bo', 'MarkerFaceColor', 'b'); % Data points for NF group
plot(X, lm_NF.Fitted, 'b-', 'LineWidth', 2); % Regression line for NF group

% Plot Control group
plot(X, Y_control, 'ro', 'MarkerFaceColor', 'r'); % Data points for Control group
plot(X, lm_control.Fitted, 'r-', 'LineWidth', 2); % Regression line for Control group

% Labels and legend
xlabel('Condition Number');
ylabel('Average Contrast Value');
title('Linear Regression of Average Contrast Values');
legend({'NF Group', 'NF Regression Line', 'Control Group', 'Control Regression Line'}, 'Location', 'Best');

hold off;

% Additional Linear Regression for NF Group (Conditions 1-4)
X_NF_1to4 = (1:4)'; % Independent variable for conditions 1 to 4
Y_NF_1to4 = mean_NF_contrast_values(1:4)'; % Dependent variable for conditions 1 to 4
lm_NF_1to4 = fitlm(X_NF_1to4, Y_NF_1to4);
fprintf('Linear Regression Results for NF Group (Conditions 1-4):\n');
disp(lm_NF_1to4);

% Perform linear regression for the Control group (Conditions 1-4)
X_control_1to4 = (1:4)'; % Independent variable for conditions 1 to 4
Y_control_1to4 = mean_control_contrast_values(1:4)'; % Dependent variable for conditions 1 to 4
lm_control_1to4 = fitlm(X_control_1to4, Y_control_1to4);
fprintf('Linear Regression Results for Control Group (Conditions 1-4):\n');
disp(lm_control_1to4);

% Visualize the additional linear regression for NF Group (Conditions 1-4) and Control Group (Conditions 1-4)
figure;
hold on;

% Plot NF group (Conditions 1-4)
plot(X_NF_1to4, Y_NF_1to4, 'bo', 'MarkerFaceColor', 'b'); % Data points for NF group (1-4)
plot(X_NF_1to4, lm_NF_1to4.Fitted, 'b-', 'LineWidth', 2); % Regression line for NF group (1-4)

% Plot Control group (Conditions 1-4)
plot(X_control_1to4, Y_control_1to4, 'ro', 'MarkerFaceColor', 'r'); % Data points for Control group (1-4)
plot(X_control_1to4, lm_control_1to4.Fitted, 'r-', 'LineWidth', 2); % Regression line for Control group (1-4)

% Labels and legend
xlabel('Condition Number');
ylabel('Average Contrast Value');
title('Linear Regression of Contrast Values (Conditions 1-4)');
legend({'NF Group (1-4)', 'NF Regression Line (1-4)', 'Control Group (1-4)', 'Control Regression Line (1-4)'}, 'Location', 'Best');

hold off;
% Additional Linear Regression for NF Group (Conditions 5-7)
X_NF_5to7 = (5:7)'; % Independent variable for conditions 5 to 7
Y_NF_5to7 = mean_NF_contrast_values(5:7)'; % Dependent variable for conditions 5 to 7
lm_NF_5to7 = fitlm(X_NF_5to7, Y_NF_5to7);
fprintf('Linear Regression Results for NF Group (Conditions 5-7):\n');
disp(lm_NF_5to7);

% Perform linear regression for the Control group (Conditions 5-7)
X_control_5to7 = (5:7)'; % Independent variable for conditions 5 to 7
Y_control_5to7 = mean_control_contrast_values(5:7)'; % Dependent variable for conditions 5 to 7
lm_control_5to7 = fitlm(X_control_5to7, Y_control_5to7);
fprintf('Linear Regression Results for Control Group (Conditions 5-7):\n');
disp(lm_control_5to7);

% Visualize the additional linear regression for NF Group (Conditions 5-7) and Control Group (Conditions 5-7)
figure;
hold on;

% Plot NF group (Conditions 5-7)
plot(X_NF_5to7, Y_NF_5to7, 'bo', 'MarkerFaceColor', 'b'); % Data points for NF group (5-7)
plot(X_NF_5to7, lm_NF_5to7.Fitted, 'b-', 'LineWidth', 2); % Regression line for NF group (5-7)

% Plot Control group (Conditions 5-7)
plot(X_control_5to7, Y_control_5to7, 'ro', 'MarkerFaceColor', 'r'); % Data points for Control group (5-7)
plot(X_control_5to7, lm_control_5to7.Fitted, 'r-', 'LineWidth', 2); % Regression line for Control group (5-7)

% Labels and legend
xlabel('Condition Number');
ylabel('Average Contrast Value');
title('Linear Regression of Contrast Values (Conditions 5-7)');
legend({'NF Group (5-7)', 'NF Regression Line (5-7)', 'Control Group (5-7)', 'Control Regression Line (5-7)'}, 'Location', 'Best');

hold off;

