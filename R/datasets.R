
#' Data Set for process macro model
#'
#'@format A data.frame with 43 rows and 7 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{X}{name of independent variable}
#'   \item{M}{names of mediator variables}
#'   \item{Y}{name of dependent variable}
#'   \item{modName}{names of moderator variables}
#'   \item{modSite}{sites of moderators}
#'   \item{pos}{position of moderators}
#'}
"pmacro"

#' Data Set for education and income
#'
#' A dataset contains measures about the teacher's knowledge, empathy and intervention about attention-deficit hyperactivity disorder(ADHD).
#'
#'@format A data.frame with 850 rows and 4 variables:
#'\describe{
#'   \item{age}{student age}
#'   \item{number}{number of students per class}
#'   \item{duration}{eduation duration}
#'   \item{income}{income}
#'}
"education"

#' Node Data Set for drawing statistical diagram of process macro model
#'
#'@format A data.frame with 327 rows and 4 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of node}
#'   \item{xpos}{x position}
#'   \item{ypos}{y position}
#'}
"nodes"

#' more models data
#'
#'@format A data.frame 2 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{no1}{model number}
#'}
"moreModels"

#' Arrow Data Set for drawing statistical diagram of process macro model
#'
#'@format A data.frame with 392 rows and 6 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of arrow}
#'   \item{start}{start node}
#'   \item{end}{end node}
#'   \item{labelpos}{position of label}
#'   \item{arrowpos}{position of arrow head}
#'}
"parrows"

#'Teacher Efficacy Data
#'
#'A dataset containing teacher efficacy, social suppot, psychological wellbeing and job stress of 247 teachers working in kindergarten
#'
#'@format A data.frame with 247 rows and 7 variables:
#'\describe{
#'  \item{age}{teacher's age. 1: 20-24, 2: 25-29, 3: 30-34, 4: 35-39, 5: 40-44, 6:45-49, 7: 50 or above }
#'  \item{marriage}{Marital Status. 0: single, 2: married}
#'  \item{children}{Parental Status. 0: no children, 1: one or more children}
#'  \item{wellbeing}{Psychological Well-being}
#'  \item{stress}{Job stress. A response syndrome of negative affects(such as anger or depression) resulting from the teacher's job}
#'  \item{efficacy}{Teacher Efficacy. A judgement of his or her capabilities to bring about desired outcomes of student engagement and learning}
#'  \item{support}{Social Support. Various resources provided by ones's interpersonal ties.}
#'}
#'@source {Cohen, S., & Hoberman, H. M. (1983). Positive events and social support as butters of life change stress. Journal of Social Applied Psychology, 13, 99-125}
#'@source {Tschannen-Moran, M., & Hoy, A. W. (2001). Teacher efficacy: Capturing an elusive construct. Teaching and teacher education, 17(7), 783-805}
#'@source {Ryff, Carol D. (1989). Happiness Is Everything, or Is It? Explorationson the Meaning of Psychological Well-Being. Journal of-Personality and Social Psychology, 57, 1069-1081}
#'@source {Kyriacou, C., & Sutcliffe, J. (1978). Teacher stress: Prevalence, sources, and symptoms. British Journal of Educational Psychology, 55, 61-64}
"teachers"

#'GBSG2
#'
#'German Breast Cancer Study Group 2
#'Description: A data frame containing the observations from the GBSG2 study.
#'
#'@format This data.frame contains the observations of 686 women:
#'\describe{
#'  \item{horTh}{hormonal therapy, a factor at two levels: no, yes}
#'  \item{age}{of the patients in years}
#'  \item{menostat}{menopausal status, a factor at two levels pre (premenopausal) and post (postmenopausal)}
#'  \item{tsize}{tumor size (in mm)}
#'  \item{tgrade}{tumor grade, a ordered factor at levels: I < II < III}
#'  \item{pnodes}{number of positive nodes}
#'  \item{progrec}{progesterone receptor (in fmol)}
#'  \item{estrec}{estrogen receptor (in fmol)}
#'  \item{time}{recurrence free survival time (in days)}
#'  \item{cens}{censoring indicator (0- censored, 1- event)}
#'@source {W. Sauerbrei and P. Royston (1999). Building multivariable prognostic and  diagnostic  models: transformation of the predictors by using fractional polynomials. Journal of the Royal Statistics Society Series A, Volume 162(1), 71–94}
#'@source {M. Schumacher, G. Basert, H. Bojar, K. Huebner, M. Olschewski, W. Sauerbrei, C. Schmoor, C. Beyerle, R. L. A. Neumann and H. F. Rauschecker for the German Breast Cancer Study Group (1994), Randomized 2×2 trial evaluating hormonal treatment and the duration of chemotherapy in node-positive breast cancer patients. Journal of Clinical Oncology, 12, 2086–2093.}
"GBSG2"

#' Teams data set
#'
#' @format A data.frame with 60 rows and 4 variables
#' \describe{
#'    \item{dysfunc}{Dysfunctional team behavior}
#'    \item{negtone}{Negative affective tone}
#'    \item{negexp}{Negative expressivity}
#'    \item{perform}{Team performance}
#' }
#' @source Cole, M. S., Walter, F., & Bruch, H. (2008). Affective mechanisms linking dysfunctional behavior to performance in work teams: A moderated mediation study. Journal of Applied Psychology, 93, 945-958.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"teams"

#' Protest dataset
#'
#'Garcia, Schmitt, Branscombe, and Ellemers (2010) report data for 129 subjects on the effects of perceived sexism on anger and liking of women's reactions to ingroup members who protest discrimination. This data set is also used as the ‘protest’ data set by Hayes (2013 and 2018). It is a useful example of mediation and moderation in regression. It may also be used as an example of plotting interactions.
#' @format A data.frame with 129 rows and 6 variables
#' \describe{
#'    \item{subnum}{subject number}
#'    \item{protest}{experimental condition, 0 = no protest, 1 = individual protest, 2 = group protest}
#'    \item{sexism}{perceived pervasiveness of sex discrimination. Means of an 8 item Modern Sexism Scale.}
#'    \item{angry}{anger toward the attorney.  “I feel angry towards Catherine".}
#'    \item{liking}{liking of the attorney. Mean rating of 6 liking ratings of the target.}
#'    \item{respappr}{appropriateness of response. Mean of four items of appropriateness of the target's response.}
#' }
#' @details
#' The reaction of women to women who protest discriminatory treatment was examined in an experiment reported by Garcia et al. (2010). 129 women were given a description of sex discrimination in the workplace (a male lawyer was promoted over a clearly more qualified female lawyer). Subjects then read that the target lawyer felt that the decision was unfair. Subjects were then randomly assigned to three conditions: Control (no protest), Individual Protest (“They are treating me unfairly") , or Collective Protest (“The firm is is treating women unfairly").
#' Participants were then asked how much they liked the target (liking), how angry they were to the target (anger) and to evaluate the appropriateness of the target's response (respappr).
#'Garcia et al(2010) report a number of interactions (moderation effects) as well as moderated-mediation effects.
#' @source Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010). Women's reactions to ingroup members who protest discriminatory treatment: The importance of beliefs about inequality and response appropriateness. European Journal of Social Psychology, 40, 733-745.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"protest"


#' PMI: Presumed Media Influence dataset
#'
#' @format A data.frame with 123 obs. of 6 variables
#' \describe{
#'    \item{cond}{front (1) or interior (0) page of the newspaper}
#'    \item{pmi}{presumed media influence}
#'    \item{import}{article is on an important topic}
#'    \item{reaction}{sugar purchase}
#'    \item{gender}{GENDER: female (0) or male (1)}
#'    \item{age}{age}
#' }
#' @source Tal-Or, N., Cohen, J., Tsafati, Y., & Gunther, A. C. (2010). Testing causal direction in the influence of presumed media influence. Communication Research, 37, 801-824.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"pmi"

#'Global Warming dataset
#'
#'@format A data.frame with 815 obs. of  7 variables
#'\describe{
#'   \item{govact}{Support for government action}
#'   \item{posemot}{Positive emotions about climate change}
#'   \item{negemot}{Negative emotions about climate change}
#'   \item{ideology}{Political ideology (conservatism), 1 = Very Liberal, 2 = Liberal, 3 = Somewhat Liberal, 4 = Moderate; Middle of the Road, 5 = Somewhat Conservative, 6 = Conservative, 7 = Very Conservative}
#'   \item{age}{Respondent age at last birthday}
#'   \item{sex}{female(0) or male(1)}
#'   \item{partyid}{1 = Democrat, 2 = Independent, 3= Republican}
#'}
#'@source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"glbwarm"


#' ESTRESS: Economic stress dataset
#'
#' @format A data.frame with 262 obs. of  7 variables
#' \describe{
#'    \item{tenure}{Company Tenure}
#'    \item{estress}{Economic stress}
#'    \item{affect}{Depressed affect}
#'    \item{withdraw}{Withdrawal intentions}
#'    \item{sex}{Male (1) or Female (0)}
#'    \item{age}{age}
#'    \item{ese}{Entrepreneurial self efficacy}
#' }
#' @source Pollack, J., VanEpps, E. M., & Hayes, A. F. (2012). The moderating role of social ties on entrepreneurs' depressed affect and withdrawal intentions in response to economic stress. Journal of Organizational Behavior, 33, 789-810.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"estress"

#' Disaster dataset
#'
#' @format A data.frame with 211 obs. of  5 variables
#' \describe{
#'    \item{id}{id}
#'    \item{frame}{Experimental condition. 0 = naturally caused disaster, 1 = climate change caused disaster}
#'    \item{donate}{Positive attitudes toward donating}
#'    \item{justify}{Negative justifications}
#'    \item{skeptic}{Climate change skepticism}
#' }
#' @source Chapman, D. A., & Little, B. (2016). Climate change and disasters: How framing affects justifications for giving or withholding aid to disaster victims. Social Psychological and Personality Science, 7, 13-20.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"disaster"


#' CASKETS dataset
#'
#' @format A data.frame with 541 obs. of  7 variables
#' \describe{
#' \item{policy}{Given information about policy (0 = No information, 1 = Told About Policy)}
#' \item{interest}{Interest in viewing casket images}
#' \item{age}{Participant age}
#' \item{educ}{Participant education level, 1 = less than high school, 2 = high school, 3 = some college, 4 = associates or technical school, 5 = bachelor degree, 6 = some graduate school, 7 = graduate degree }
#' \item{male}{Participant sex (0 = female, 1 = male)}
#' \item{conserv}{Participant social conservatism}
#' \item{kerry}{Kerry or Bush supporter, 0 = bush supporter, 1 = kerry supporter}
#' }
#' @source Hayes, A. F., & Reineke, J. B. (2007). The effects of government censorship of war-related news coverage on interest in the censored coverage: A test of competing theories. Mass Communication and Society, 10, 423-438
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"caskets"

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","pmacro","parrows","moreModels","nodes"))
