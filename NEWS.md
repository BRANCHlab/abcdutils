# abcdutils 0.1.1

# abcdutils 0.1.0

* Initial package release, including:
    * utility functions:
        * `dummy`: generate dummy variables
        * `col_to_num`: convert specific factor columns to numeric
        * `col_to_fac`: convert specific character columns to numeric
    * tbi functions:
        * `rename_tbi`: provide meaningful column names to abcd_otbi01.txt
        * `original_otbi_names`: view original abcd_otbi01.txt column names
        * `identify_all_tbi`: label mTBIs and moderate/severe TBIs
        * `identify_mtbi`: identify which mechanism of injury counts as an mTBI
        * `identify_mtbi_times`: identify time since most recent mTBI
        * `identify_latest_mtbi_mechanism`: identify mechanism of injury for latest mTBI
        * `identify_num_mtbi`: identify how many mTBIs a subject sustained
        * `identify_latest_mtbi_loc`: identify LOC time for latest mTBI
        * `identify_latest_mtbi_mem_daze`: identify if memory loss / feeling dazed or confused occurred for latest mTBI
        * `detail_mtbi`: apply all other TBI functions to a dataframe
    * extraction functions:
        * `get_mtbi_subjects`
        * `get_cbcl_aggressive_r`
        * `get_cbcl_anxiety_r`
        * `get_cbcl_attention_r`
        * `get_cbcl_depress_r`
        * `get_cbcl_dizzy`
        * `get_cbcl_headaches`
        * `get_cbcl_overtired`
        * `get_cbcl_sleeping_less`
        * `get_cbcl_sleeping_more`
        * `get_cbcl_vomiting`
        * `get_cort_sa`
        * `get_cort_t`
        * `get_exercise`
        * `get_family_function`
        * `get_full_sleep_df`
        * `get_gord_cor`
        * `get_gord_var`
        * `get_headaches`
        * `get_income`
        * `get_loneliness`
        * `get_mtbi_age`
        * `get_mtbi_count`
        * `get_mtbi_loc`
        * `get_mtbi_mechanism`
        * `get_mtbi_mem_daze`
        * `get_nihtbx_cardsort_fc`
        * `get_nihtbx_list_fc`
        * `get_nihtbx_pattern_fc`
        * `get_parent_psychopathology`
        * `get_prosocial_behaviour`
        * `get_pubertal_status`
        * `get_race`
        * `get_screen_time`
        * `get_sex`
        * `get_sports_and_activities`
        * `get_subc_cor`
        * `get_subc_v`
        * `get_subc_var`
        * `get_wmnd`
    * subsetting functions:
        * `filter_timepoint`: (isolate to specific data collection year)
        * `filter_subjects`: (isolate to provided list of subjects)
    * data dictionary functions:
        * `remove_dd`: remove the data dictionary from a dataframe
        * `search_dd`: open the data dictionary search results for a string
        * `abcd_dd`: open the data dictionary page for a data item's short name
    * plotting functions:
        * `vis_missing_by_df`: visualize missing data across multiple dataframes
* Added a `NEWS.md` file to track changes to the package.
