% Define the path to the main project directory
main_project_dir = '/Users/test/Desktop/Bachelorarbeit/BA_analyses/Data/fMRI_final';
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
combined_roi_mask = (roi_mask1 > 0) | (roi_mask2 > 0);

% Save the combined ROI mask to a new file
V_combined_roi = V_roi1; % Copy the header information from one of the ROI masks
V_combined_roi.fname = fullfile(roi_dir, 'Combined_Hippocampus_ROI.nii');
spm_write_vol(V_combined_roi, combined_roi_mask);

% Path to the sample contrast image
sample_con_file = fullfile(main_project_dir, 'Subj_001', 'Count_Upreg_fDisp', 'con_0001.nii');
V_sample = spm_vol(sample_con_file);

% Reslice the combined ROI mask to the space of the sample contrast image
flags = struct('interp', 1, 'mask', 1, 'mean', 0, 'which', 1, 'wrap', [0 0 0], 'prefix', 'r');
spm_reslice({V_sample.fname, V_combined_roi.fname}, flags);

% Load the resliced combined ROI mask
resliced_roi_mask_path = fullfile(roi_dir, 'rCombined_Hippocampus_ROI.nii');
V_resliced_roi = spm_vol(resliced_roi_mask_path);
resliced_combined_roi_mask = spm_read_vols(V_resliced_roi);

% Ensure resliced combined ROI mask is non-empty
fprintf('Number of non-zero elements in resliced combined ROI mask: %d\n', nnz(resliced_combined_roi_mask));
