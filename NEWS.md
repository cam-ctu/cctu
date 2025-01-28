# 0.8.4

Added in km_ggplot() function to produce publication-quality Kaplan-Meier figures with error bands and table underneath.

# 0.8.3

Fixed a bug with write_docx() in the multi-line headers are now completely visible. Testing added of docx outputs.

Added in options(cctu_output) and optinos(cctu_source_local) to change the default arguents for write_xx() and source()

Updated Vignette


# 0.8.2

Added write_docx() which creates directly a fully compliant OfficeOpen docx file,
no subsequent steps needed, and it does open on the online office/word tools.


# 0.8.1 

Fixing the github continuous integration. Minor fixes to rbind_space, data_table_summary


#  0.8.0

Added in the regression_table() generic to print a nice tidy table
to present regression models. Minor improvements and updates to other functions including cttab and write_plot.



# V 0.7.6

-	The apply_macro_dict() function is faster now. By default, evaluating whether a category variable's type is numeric before converting is skipped.
-	Table and figure numbers are locked, saving Word to PDF will not change the numbers.
-	Figures are embedded in the document. No need to perform “save picture in document”.
-	Dynamically add footnotes to the tables and figures in write_table() and write_ggplot().
-	There’s a new function write_plot() to save figures other than the ggplot family, KM-plot from survminer for example. 
-	Bug fix: headings will be used if it is provided in the write_table().
- p_format will round pvalues and convert to <0.001 as a character variable.

