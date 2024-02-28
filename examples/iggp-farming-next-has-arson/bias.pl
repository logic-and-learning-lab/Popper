allow_singletons.
constant(agent_red, agent).
constant(agent_blue, agent).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(mypos_4, mypos).
constant(int_0, int).
constant(int_5, int).
constant(int_6, int).
constant(int_7, int).
constant(int_8, int).
constant(int_9, int).
constant(int_10, int).
constant(int_11, int).
constant(int_12, int).
constant(int_13, int).
constant(int_14, int).
constant(int_15, int).
constant(int_16, int).
constant(int_17, int).
constant(int_18, int).
constant(int_19, int).
constant(int_20, int).
constant(int_21, int).
constant(int_22, int).
constant(int_23, int).
constant(int_24, int).
constant(int_25, int).
constant(int_26, int).
constant(int_27, int).
constant(int_28, int).
constant(int_29, int).
constant(int_30, int).
constant(int_31, int).
constant(int_32, int).
constant(int_33, int).
constant(int_34, int).
constant(int_35, int).
constant(int_36, int).
constant(int_37, int).
constant(int_38, int).
constant(int_39, int).
constant(int_40, int).
constant(int_41, int).
constant(int_42, int).
constant(int_43, int).
constant(int_44, int).
constant(int_45, int).
constant(int_46, int).
constant(int_47, int).
constant(int_48, int).
constant(int_49, int).
constant(int_50, int).
constant(int_51, int).
constant(int_52, int).
constant(int_53, int).
constant(int_54, int).
constant(int_55, int).
constant(int_56, int).
constant(int_57, int).
constant(int_58, int).
constant(int_59, int).
constant(int_60, int).
constant(int_61, int).
constant(int_62, int).
constant(int_63, int).
constant(int_64, int).
constant(int_65, int).
constant(int_66, int).
constant(int_67, int).
constant(int_68, int).
constant(int_69, int).
constant(int_70, int).
constant(int_71, int).
constant(int_72, int).
constant(int_73, int).
constant(int_74, int).
constant(int_75, int).
constant(int_76, int).
constant(int_77, int).
constant(int_78, int).
constant(int_79, int).
constant(int_80, int).
constant(int_81, int).
constant(int_82, int).
constant(int_83, int).
constant(int_84, int).
constant(int_85, int).
constant(int_86, int).
constant(int_87, int).
constant(int_88, int).
constant(int_89, int).
constant(int_90, int).
constant(int_91, int).
constant(int_92, int).
constant(int_93, int).
constant(int_94, int).
constant(int_95, int).
constant(int_96, int).
constant(int_97, int).
constant(int_98, int).
constant(int_99, int).
constant(int_100, int).
constant(season_type_summer, season_type).
constant(season_type_fall, season_type).
constant(season_type_winter, season_type).
constant(season_type_spring, season_type).
constant(action_noop, action).
head_pred(next_has_arson,2).
body_pred(true_control,2).
body_pred(true_season,2).
body_pred(true_year_first_player,2).
body_pred(true_year_second_player,2).
body_pred(true_score,3).
body_pred(true_step,2).
body_pred(true_plowed,4).
body_pred(true_sown,4).
body_pred(true_ripe,4).
body_pred(true_has_arson,2).
body_pred(input,2).
body_pred(input_plow_row,2).
body_pred(input_plow_col,2).
body_pred(input_sow_row,2).
body_pred(input_sow_col,2).
body_pred(input_water_row,2).
body_pred(input_water_col,2).
body_pred(input_harvest_row,2).
body_pred(input_harvest_col,2).
body_pred(input_arson_row,2).
body_pred(input_arson_col,2).
body_pred(does,3).
body_pred(does_plow_row,3).
body_pred(does_plow_col,3).
body_pred(does_sow_row,3).
body_pred(does_sow_col,3).
body_pred(does_water_row,3).
body_pred(does_water_col,3).
body_pred(does_harvest_row,3).
body_pred(does_harvest_col,3).
body_pred(does_arson_row,3).
body_pred(does_arson_col,3).
body_pred(role,1).
body_pred(get_arson,1).
body_pred(index,1).
body_pred(point_succ_two,2).
body_pred(season_list,1).
body_pred(succ,2).
type(true_control,(ex,agent)).
type(true_season,(ex,season_type)).
type(true_year_first_player,(ex,agent)).
type(true_year_second_player,(ex,agent)).
type(true_score,(ex,agent,int)).
type(true_step,(ex,int)).
type(true_plowed,(ex,agent,mypos,mypos)).
type(true_sown,(ex,agent,mypos,mypos)).
type(true_ripe,(ex,agent,mypos,mypos)).
type(true_has_arson,(ex,agent)).
type(next_has_arson,(ex,agent)).
type(input,(agent,action)).
type(input_plow_row,(agent,mypos)).
type(input_plow_col,(agent,mypos)).
type(input_sow_row,(agent,mypos)).
type(input_sow_col,(agent,mypos)).
type(input_water_row,(agent,mypos)).
type(input_water_col,(agent,mypos)).
type(input_harvest_row,(agent,mypos)).
type(input_harvest_col,(agent,mypos)).
type(input_arson_row,(agent,mypos)).
type(input_arson_col,(agent,mypos)).
type(does,(ex,agent,action)).
type(does_plow_row,(ex,agent,mypos)).
type(does_plow_col,(ex,agent,mypos)).
type(does_sow_row,(ex,agent,mypos)).
type(does_sow_col,(ex,agent,mypos)).
type(does_water_row,(ex,agent,mypos)).
type(does_water_col,(ex,agent,mypos)).
type(does_harvest_row,(ex,agent,mypos)).
type(does_harvest_col,(ex,agent,mypos)).
type(does_arson_row,(ex,agent,mypos)).
type(does_arson_col,(ex,agent,mypos)).
type(role,(agent,)).
type(get_arson,(int,)).
type(index,(mypos,)).
type(point_succ_two,(int,int)).
type(season_list,(season_type,)).
type(succ,(int,int)).

:-
	clause(C),
	#count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
