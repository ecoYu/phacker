{smcl}
{it:v. 1.0}


{title:Title}

{p 4 4 2}
{hi:phacker} —— (Alpha edition) Helps you to quickly screen for control variables that keep all variables at a certain level of significance in regression.{p_end}


{title:Syntax}

{p 4 4 2}
{cmdab:phacker} y pervars [if] [in], 
{cmdab:m:ethod:}{cmd:(}reg{cmd:)}
{cmdab:sig:ma:}{cmd:(}0.05{cmd:)}
[ {cmd:,} {it:options} ]
{p_end}


{synoptset 24 tabbed}{...}
{marker opts}{...}
{synopthdr}
{synoptline}
{synopt:{opt varlist(#)}}specifies the dependent variable(y) and the variables(prevars) that you want to screen for
{p_end}
{synopt:{opt m:ethod(#)}}specifies the estimator you want to use,usually in {hi:reg}
{p_end}
{synopt:{opt sig:ma(#)}}specifies the level of significance, 0.05 is a commonly used value
{p_end}
{synopt:{opt max(#)}}specifies maximum number of items in a tuple, that is, the maximum number of prevars you need
{p_end}
{synopt:{opt min(#)}}specifies minimum number of items in a tuple, that is, the minimum number of prevars you need
{p_end}
{synopt:{opt h:ead(#)}}displays list header every # lines, the default value is 10. If you want to get all outcomes, please add {hi:head(all)} or {hi:h(all)}
{p_end}
{synopt:{opt save}}saves the final result list as a dta file by adding {hi:save}. And by typing "pwd" on the stata, you can see where the file is saved.
{p_end}
{synopt:{opt keep:var(#)}}specifies those variables that remains involved in regression all the time
{p_end}
{synopt:{opt fe:ffect(#)}}specifies those variables to which you want to add a fixed effect in regression
{p_end}
{synopt:{opt obs:ervation(#)}}sets obs observations, the default value is number of samples + 10000. Sometimes larger value may be required
{p_end}
{synopt:{opt o:thers(#)}}specifies the additional options in regression, like adding {hi:o(absorb(#))} when you're using {hi:reghdfe}
{p_end}
{synoptline}
{p2colreset}{...}


{title:Description}

{p 4 4 2}
If your boss or supervisor has an odd penchant for regression p-values, try {cmd:phacker}! It is built on top of command {it:tuples}, the first half of my command is based entirely on {it:tuples} work. {p_end}


{title:Results}

{p 4 4 2}
After running {hi:phacker}, you will see a list. The first column is R square, the second column is a √ or X (if all variables are statistically significant, there will be √ ), and the third column is the corresponding combination of variables.
{p_end}


{title:Examples}

{p 4 4 2} * Selecting the combination from: {hi:foreign} {hi:trunk} {hi:weight} {hi:gear_ratio} {hi:headroom} {hi:displacement}.
{p_end}

{p 4 4 2}{inp:.}
{stata `"sysuse auto.dta, clear"'}
{p_end}
{p 4 4 2}{inp:.}
{stata `"phacker mpg foreign trunk weight gear_ratio headroom displacement, m(reg) keep(price) sig(0.01) max(5) min(2) head(15) fe(i.length i.turn) save o(robust)"'}
{p_end}


{title:Author}

{p 4 4 2}
{cmd:Bryce Yu}{break}
Renmin University of China.{break}
E-mail: {browse "mailto:yu.xiaokun@outlook.com":yu.xiaokun@outlook.com}. {break}


{title:Acknowledgments}

{p 4 4 2}
Thanks to Joseph N. Luchman, Daniel Klein, Nicholas J. Cox for their command {it:tuples}. This is a project of great creativity，I love it！
{p_end}
{p 4 4 2}
Thanks to Miss Jin, my bosom friend, for providing me with this opportunity to create.
{p_end}
{p 4 4 2}
Thanks to the sponsor of this project, even though he/she doesn't exist.
{p_end}

