
% Assuming you have extracted contrast values for both groups:
% NF_contrast_values_007 - for the original group
% sham_contrast_values_007 - for the sham group

% Perform an independent samples t-test between the NF group and Sham group
[h, p, ci, stats] = ttest2(NF_contrast_values_007, sham_contrast_values_007);

% Display the results
fprintf('Independent samples t-test results:\n');
fprintf('t-value: %.3f\n', stats.tstat);
fprintf('p-value: %.3f\n', p);
fprintf('Confidence Interval: [%.3f, %.3f]\n', ci);
fprintf('Mean contrast value (NF group): %.3f\n', mean(NF_contrast_values_007));
fprintf('Mean contrast value (Sham group): %.3f\n', mean(sham_contrast_values_007));

% Calculate Cohen's d for the comparison
mean_diff = mean(NF_contrast_values_007) - mean(sham_contrast_values_007);
pooled_std = sqrt(((length(NF_contrast_values_007)-1)*var(NF_contrast_values_007) + (length(sham_contrast_values_007)-1)*var(sham_contrast_values_007)) / (length(NF_contrast_values_007) + length(sham_contrast_values_007) - 2));
cohen_d = mean_diff / pooled_std;

fprintf('Cohen''s d: %.3f\n', cohen_d);

% Compute the standard error of Cohen's d
n_NF = length(NF_contrast_values_007);
n_sham = length(sham_contrast_values_007);
se_d = sqrt((1/n_NF + 1/n_sham) + (cohen_d^2 / (2*(n_NF + n_sham - 2))));

% Calculate the confidence interval (95%)
ci_d_lower = cohen_d - 1.96 * se_d;
ci_d_upper = cohen_d + 1.96 * se_d;

fprintf('Cohen''s d Confidence Interval: [%.3f, %.3f]\n', ci_d_lower, ci_d_upper);

% Create a scatter plot showing the mean with error bars for both groups
figure;
scatter(ones(length(NF_contrast_values_007), 1), NF_contrast_values_007, 'filled', 'r'); % Scatter points for NF group
hold on;
scatter(2*ones(length(sham_contrast_values_007), 1), sham_contrast_values_007, 'filled', 'b'); % Scatter points for Sham group
errorbar(1, mean(NF_contrast_values_007), std(NF_contrast_values_007)/sqrt(length(NF_contrast_values_007)), 'k', 'LineWidth', 2); % Error bar for NF group
errorbar(2, mean(sham_contrast_values_007), std(sham_contrast_values_007)/sqrt(length(sham_contrast_values_007)), 'k', 'LineWidth', 2); % Error bar for Sham group
plot(1, mean(NF_contrast_values_007), 'b*', 'MarkerSize', 10, 'LineWidth', 2); % Mean marker for NF group
plot(2, mean(sham_contrast_values_007), 'r*', 'MarkerSize', 10, 'LineWidth', 2); % Mean marker for Sham group

% Customize the plot
ylabel('Contrast Value in Hippocampus');
title('Hippocampus Activation: Mean and Confidence Interval with Data Points');
xlim([0.5, 2.5]);
xticks([1 2]);
xticklabels({'NF group', 'Sham group'});
