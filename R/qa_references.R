qa_references <- function() {
  c(
    bibentry(
      bibtype = "Article",
      key = "ibrahim2019",
      title = "Model-{{Based Conditional Weighted Residuals Analysis}} for {{Structural Model Assessment}}",
      volume = "21",
      issn = "1550-7416",
      abstract = "Nonlinear mixed effects models are widely used to describe longitudinal data to improve the efficiency of drug development process or increase the understanding of the studied disease. In such settings, the appropriateness of the modeling assumptions is critical in order to draw correct conclusions and must be carefully assessed for any substantial violations. Here, we propose a new method for structure model assessment, based on assessment of bias in conditional weighted residuals (CWRES). We illustrate this method by assessing prediction bias in two integrated models for glucose homeostasis, the integrated glucose-insulin (IGI) model, and the integrated minimal model (IMM). One dataset was simulated from each model then analyzed with the two models. CWRES outputted from each model fitting were modeled to capture systematic trends in CWRES as well as the magnitude of structural model misspecifications in terms of difference in objective function values ({$\\Delta$}OFVBias). The estimates of CWRES bias were used to calculate the corresponding bias in conditional predictions by the inversion of first-order conditional estimation method's covariance equation. Time, glucose, and insulin concentration predictions were the investigated independent variables. The new method identified correctly the bias in glucose sub-model of the integrated minimal model (IMM), when this bias occurred, and calculated the absolute and proportional magnitude of the resulting bias. CWRES bias versus the independent variables agreed well with the true trends of misspecification. This method is fast easily automated diagnostic tool for model development/evaluation process, and it is already implemented as part of the Perl-speaks-NONMEM software.",
      language = "eng",
      number = "3",
      journal = "The AAPS journal",
      doi = "10.1208/s12248-019-0305-2",
      author = c(
        person(given = c("Moustafa", "M.", "A."),
               family = "Ibrahim"),
        person(given = "Sebastian",
               family = "Ueckert"),
        person(given = "Svetlana",
               family = "Freiberga"),
        person(given = c("Maria", "C."),
               family = "Kjellsson"),
        person(given = c("Mats", "O."),
               family = "Karlsson")
      ),
      month = "feb",
      year = "2019",
      keywords = "conditional weighted residuals,diagnostics,model evaluation,nonlinear mixed effects models,prediction bias,structural model",
      pages = "34",
      pmid = "30815754",
      pmcid = "PMC6394649"
    ),
    bibentry(
      bibtype = "Article",
      key = "svensson2014",
      title = "Use of a Linearization Approximation Facilitating Stochastic Model Building",
      volume = "41",
      issn = "1573-8744",
      abstract = "The objective of this work was to facilitate the development of nonlinear mixed effects models by establishing a diagnostic method for evaluation of stochastic model components. The random effects investigated were between subject, between occasion and residual variability. The method was based on a first-order conditional estimates linear approximation and evaluated on three real datasets with previously developed population pharmacokinetic models. The results were assessed based on the agreement in difference in objective function value between a basic model and extended models for the standard nonlinear and linearized approach respectively. The linearization was found to accurately identify significant extensions of the model's stochastic components with notably decreased runtimes as compared to the standard nonlinear analysis. The observed gain in runtimes varied between four to more than 50-fold and the largest gains were seen for models with originally long runtimes. This method may be especially useful as a screening tool to detect correlations between random effects since it substantially quickens the estimation of large variance-covariance blocks. To expedite the application of this diagnostic tool, the linearization procedure has been automated and implemented in the software package PsN.",
      language = "eng",
      number = "2",
      journal = "Journal of Pharmacokinetics and Pharmacodynamics",
      doi = "10.1007/s10928-014-9353-5",
      author = c(
        person(given = c("Elin", "M."),
               family = "Svensson"),
        person(given = c("Mats", "O."),
               family = "Karlsson")
      ),
      month = "apr",
      year = "2014",
      keywords = "Humans,Models; Biological,Pharmacokinetics,Software,Nonlinear Dynamics,Stochastic Processes,Ethambutol,Imidazoles,Pefloxacin",
      pages = "153-158",
      pmid = "24623084",
      pmcid = "PMC3969514"
    )
  )
}
