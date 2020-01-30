#' Content sections for the QA report
#'
#' These function generate the different content section for the QA report
#'
#' @return Latex/HTML for the section
#' @name  qa_report_content
#' @export
qa_report_content_intro <- function(){
  if(!knitr::is_html_output()) return(invisible())
  about_report_panel <- qa_ui_manual_panel(
    title = "The QA report",
    htmltools::tags$p(
      "This report aims to be simultaneously concise and intelligible. To achieve this, it allows you to switch
      between two modes, Result Mode and Tutorial Mode. The former focuses on the results and targets users with
      some experience with QA. The Tutorial Mode adds additional text to the different sections, intended to
      explain what is shown in the report and how it should be interpreted. The display of the results is
      identical between the different modes. Currently, you are in Tutorial Mode, and you can use the button
      in the navigation bar to return to the Result Mode."
   ),
   htmltools::tags$p(
     "In addition to switching between modes, the navigation bar also allows you to jump to a specific section directly.
     A click on the \"PsN QA\" link will bring you back to the top. "
   ),
   htmltools::tags$p(
     "In Tutorial Mode, the report is structured using three different panels, distinguishable by icon:",
     qa_ui_icon("glyphicon-user"),
     " indicates information that provides an introduction to QA as one would typically find in a user guide, ",
     qa_ui_icon("glyphicon-cog"),
     " indicates technical details that are not strictly necessary to use QA, and ",
     qa_ui_icon("glyphicon-flash"),
     " indicates actionable advice that helps to advance the model building process.",
     # "Throughout the report you
     # will also find the ",
     #  qa_ui_icon("glyphicon-info-sign"),
     # " symbol next to figures and tables. Clicking on it will reveal additional explanations
     # for this specific element. Clicking the symbol again will hide the information."
     ),
   htmltools::tags$p(
     "The QA report is self-contained, you can share it with a colleague by merely sending the HTML file. "
   )
  )

  about_qa_panel <- qa_ui_manual_panel(
    title = "The QA tool",
    htmltools::tags$p(
      "QA is a model diagnostic tool that evaluates a pharmacometric model in regards to the six aspects:
      1. structural model, 2. parameter variability model, 3. covariates, 4. residual error model, 5. influential
      individuals, and 6. outliers. In contrast to most model diagnostics, it works in a confirmatory manner
      by testing specific model modifications and quantifying the expected improvement in fit. Throughout
      a typical QA run, the equivalent of several dozen",
      htmltools::tags$abbr(title="non-linear mixed effect models", class="initialism", "NLMEMs"),
      "are run. However, QA never actually estimates the full NLMEM but uses faster and more stable
      proxy-models to perform the evaluation (see the following panel for details)."
    ),
    htmltools::tags$p(
      "The expected impact of the model modifications on the fit is quantified in terms of OFV improvement (dOFV).
      The larger the expected change in OFV, the more the model is expected to improve when this modification
      is implemented. All tested modifications have the submitted model (or its proxy-equivalents to be precise)
      as a base model and, hence, should result in positive OFV changes. Negative OFVs indicate numerical
      difficulties such as local minima and should be ignored."
    )
  )
  proxy_models_panel <- qa_ui_technical_panel(
    title = "Proxy models",
    htmltools::tags$p(
      "Two types of proxy-models are used within QA: linearized and residual-based models. Both types are
      several magnitudes faster than the original model and considerably more stable. All model modifications
      tested as part of QA utilize one of the two. For a given modification, the model proxy of the submitted
      model serves as the reduced model and the model proxy with the modification as the full model.
      The difference in OFV between full and reduced model yields the dOFV value reported.
      "
    ),
    htmltools::tags$h5("Linearized Model"),
    htmltools::tags$p(
      "The linearized model is obtained through first-order Taylor expansion of the submitted model with respect to the
      subject-specific random effects (ETA variables). In addition to that, all non-variability population parameters
      (THETA variables) are fixed to the values provided to QA. In consequence, the resulting model can be considerably
      simplified and, for example, does not require any differential equations. In contrast to the residual-based models,
      however,  the linearized model still depends on ETA variables and, hence, can serve as a basis for the evaluation of
      transformations of them.",
      qa_ui_cite("svensson2014")
    ),
    htmltools::tags$h5("Residual-based Model"),
    htmltools::tags$p(
      "The residual-based proxy model describes the conditional weighted residuals of the submitted model, i.e., it does not
      directly model the original data. The model also does not contain any reference to the fixed and random effects of the
      submitted model. However, it can still be used to investigate modifications of the error model and detect structural bias.
      The residual-based model is also extremely stable and fast.",
      qa_ui_cite("ibrahim2019")
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This is a dynamic report generated with the PsN QA tool, it provides comprehensive diagnostics and
      unique insights into a pharmacometric model. Switch to the \"Tutorial Mode\" using the ",
      qa_ui_icon("glyphicon-education"),
      " button in the navigation bar to learn more about this report."
    ),
    qa_ui_tmode_content(
      about_report_panel,
      about_qa_panel,
      proxy_models_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_overview <- function(){

  about_overview_panel <- qa_ui_manual_panel(
    title = "Overview section",
    htmltools::tags$p(
      "The overview section provides a high-level summary of the results obtained. For each of the six model
      aspects, the most impactful modifications are listed together with the expected improvement in
      ",
      htmltools::tags$abbr(title="objective function value", "OFV"),
      " and the number of parameters required to implement them. Substantial improvements warrant further investigation
      in the details sections of the report. Low values throughout this table indicate a well-performing model."
    )
  )

  htmltools::tags$div(
    qa_ui_tmode_content(
      about_overview_panel
    ),
    htmltools::tags$p(
      "The following overview table lists the most impactful modifications identified by QA. More details for each result
      can be found in the individual sections below."
    )
  )
}


#' @export
#' @rdname qa_report_content
qa_report_content_structural <- function(){

  about_structural_panel <- qa_ui_manual_panel(
    title = "Structural section",
    htmltools::tags$p(
      "The structural model misspecifications are evaluated in QA as a function of different ",
      htmltools::tags$abbr(title="independent variables", "IDVs"),
      "TIME and PRED are always generated, TAD if the variable is available, and an additional
      one can be added using the command line option ",
      htmltools::tags$code("-resmod_idv=VAR"),
      ". For each IDV separately, QA splits the data into bins with an equal amount of observations.
      It then uses the residuals of the submitted model to estimate the difference between
      observations and model predictions. The impact
      of the estimated difference is presented using two complementary measures: difference in
      OFV and the mean bias per bin. The former is an estimate of how much the OFV of the
      submitted model could improve if the bias is addressed and the latter quantifies the bias
      on the same scale as the observations."
    ),
    htmltools::tags$p(
      "  This report contains a subsection for each IDV that was used, with an identical layout
      within the subsections. First, a table indicates what type of residuals and what IDV
      were used, and what the expected OFV improvement is. The following table lists the bias
      on the residual as well as on the observation scale for each bin. The observation scale
      is indicated as ",
      htmltools::tags$abbr(title="conditional PRED", "CPRED"),
      "(following NONMEM terminology). The first plot in each subsection visualizes the bias,
      on the scale of the observations, for each bin and, hence, allows an easier identification of
      trends. Finally, each subsection also contains a before/after VPC that compares a generalized
      VPC for the submitted model (or its linear proxy to be precise) with one where the model has been
      corrected to remove the identified bias. "
    )
  )

  resmod_panel <- qa_ui_technical_panel(
    title = "CWRES-based structural model assessment",
    htmltools::tags$p(
      "The structural model is assessed using a residual-based proxy model. For the estimation of the
      bias the data is split, based on the independent variable, into bins with equal amount of
      observations. For each bin, the mean difference between model predictions and observations
      is estimated using model residuals (see subsequent box for details). ",
      qa_ui_cite("ibrahim2019")
    ),
    htmltools::tags$p(
      "The structural model is assessed using a residual-based proxy model that estimates the
      bias in residual for each bin. "
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Identifying structural misspecifications",
    htmltools::tags$p(
      "A large dOFV relative to the number of bins (e.g., larger than 20 for 9 bins) is an indicator
       of substantial bias. The plots of estimated bias vs. independent variable can then help to
      identify where the misspecification lies. If, for example, bias is largest for early time
      points in a PK model, it could indicate that the absorption model is not describing the data
      well. As the interpretation, the specific actions will also very much depend on the type of
      the submitted model. Generally, however, a more flexible model should be tested when substantial
      bias has been identified."
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section aims at diagnosing the structural component of the model. It does so by estimating
      the mean difference between model predictions and observations as a function of several
      independent variables."
    ),
    qa_ui_tmode_content(
      about_structural_panel,
  #    resmod_panel,
      advice_panel
    )
  )
}


#' @export
#' @rdname qa_report_content
qa_report_content_parvar <- function(){

  about_parvar_panel <- qa_ui_manual_panel(
    title = "Parameter variability section",
    htmltools::tags$p(
      "This section shows the results of testing up to five different modifications to the parameter variability model:
      (1) addition of a full OMEGA block, (2) Box-Cox transformation of the random effects, (3) t-distribution transformation,
      (4) inclusion of inter-occasion variability, and (5) inclusion of additional ETA variables. A dedicated section
      details the results of applying these modifications. Sometimes, a modification might be skipped if it was not requested
      or is not applicable; the corresponding subsection will then detail why no results were obtained. "
    )
  )

  parvar_technical_panel <- qa_ui_technical_panel(
    title = "Linearization-based parameter variability model building",
    htmltools::tags$p(
      "All modifications presented in this section use the linearized proxy as their base model and are implemented as they
      would be for the non-linear case (except for the addition of ETAs). The resulting modified models contain additional
      parameters, such as new covariance terms in the case of the full block and new fixed effects in the case of the Box-Cox
      transformation. NONMEM is then run to estimate these parameters and to obtain the OFV for the modified model.",
      qa_ui_cite("svensson2014")
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section evaluates the variability model of the parameters with respect to a number of
      different modifications. Each of the following subsections lists the results for one of the modifications. "
    ),
    qa_ui_tmode_content(
      about_parvar_panel,
      parvar_technical_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_full_block <- function(){
  advice_panel <- qa_ui_advice_panel(
    title = "Implementing a full OMEGA block",
    htmltools::tags$p(
      "If the table below indicates a significant improvement, you should add a full OMEGA block to your model. The
      easiest way to do that is to use the PsN transform tool as follows:",
      PsNR::qa_ui_console("transform full_block run1.mod")
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of including a full block correlation structure in the submitted model. "
    ),
    qa_ui_tmode_content(
      advice_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_box_cox <- function(){

  intro_panel <- qa_ui_manual_panel(
    title = "The Box-Cox transformation",
    htmltools::tags$p(
      "Sometimes, the assumption of a normal distribution for the random effects in an NLME does not hold. Right or left
      skewness is one possible reason, and estimating an optimal Box-Cox transformation can help to identify these cases.
      QA estimates the shape parameter Lambda for all ETA variables in the submitted model (including ETAs with FIX and
      SAME in the corresponding $OMEGA record). "
    ),
    htmltools::tags$p(
      "In the overview table below, Lambda and the total improvement in OFV are presented. A Lambda value below 0 indicates
      a left-skewed and a value above 0, indicates a right-skewed distribution of random effects.
      As Lambda approaches 0, the transformation approaches the identity transformation. In addition to the overview table,
      the section also contains a visualization of the ETA density before and after applying the transformation. "
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Box-Cox transforming a random effect",
    htmltools::tags$p(
      "The dOFV indicates the expected improvement in OFV if all ETA variables were to be transformed, resulting in
      one additional parameter per ETA. Base and transformed model are nested and, hence, the classical chi-square
      distribution based significance levels can serve as guidance. "),
    htmltools::tags$p("Sometimes only some of the ETA variables show a Lambda parameter considerably different from 0.
    In this case, for reasons of parsimony, only those should be transformed. "),
    htmltools::tags$p("You can use the PsN transform tool to implement the Box-Cox transformation for the ETA variables
    of your choice. For example, to implement the transformation for ETAs one to three, you can use the command:",
    ),
    PsNR::qa_ui_console("transform boxcox run1.mod -etas=1,2,3")
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of applying a Box-Cox transformation to the ETA variables in the submitted model. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      advice_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_tdist <- function(){

  intro_panel <- qa_ui_manual_panel(
    title = "The t-distribution transformation",
    htmltools::tags$p(
      "In addition to skewness, heavy tails are another reason why the assumption of a normal distribution for the ETA
      variables might not hold. This can be evaluated by estimating a t-distribution transformation. In, QA this
      is done for all ETA variables in the submitted model (including ETAs with FIX and SAME in the corresponding
      $OMEGA record). "
    ),
    htmltools::tags$p(
      "The transformation has one parameter, interpretable as the degrees of freedom (DF) for the corresponding
      t-distribution. This parameter, together with the expected OFV improvement when implementing the transformation
      for all variables, is shown in the overview table. The DF parameter is positive, and smaller values indicate heavier
      tails. The t-distribution transformation approaches the identity transformation (i.e., does not have any effect)
      as the DF parameter approaches infinity. However, already beyond a DF value of 10, the effect of the transformation
      is ignorable in practice. As for the Box-Cox transformation, a graph comparing the density of the ETA distribution
      before and after applying the transformation is included in this section.  "
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Applying the t-distribution transformation",
    htmltools::tags$p(
      "The dOFV indicates the expected improvement in OFV if all ETA variables were to be transformed, resulting in
      one additional parameter per ETA. While base and transformed model are not fully nested (the normal distribution is
      approached when the degrees of freedom approach infinity), the chi-square distribution based significance levels
      can still serve as approximate guidance. "),
    htmltools::tags$p("To keep the model parsimonious, you should only transform the ETA variables that have a low degree of
    freedom estimate (e.g., less than 10). "),
    htmltools::tags$p("You can use the PsN transform tool to implement the t-distribution transformation for the ETA variables
    of your choice. For example, to implement the transformation for ETAs one to three, you can use the command:",
    ),
    PsNR::qa_ui_console("transform tdist run1.mod -etas=1,2,3")
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of applying a t-distribution transformation to the ETA variables in the submitted model. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      advice_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_iov <- function(){

  intro_panel <- qa_ui_manual_panel(
    title = "Inter-occasion variability",
    htmltools::tags$p(
      "Inter-occasion variability (IOV) constitutes an additional level of within-subject variability in an NLME. QA tests the
      impact of including IOV in the submitted model only if the data record indicating the different occasions is provided
      on the command line (e.g., ",
      htmltools::tags$code("-occ=VAR"),
      "). In this case, IOV is included for each ETA variable in the submitted model resulting in one additional variance
      parameter per ETA variable. IOV is not added if the submitted model already contains a SAME statement in the $OMEGA
      record."
    ),
    htmltools::tags$p(
      "The results below show the total expected improvement in OFV as well as the estimated inter-occasion and inter-individual
      variability in comparison with the values from the submitted model."
    )
  )

 advice_panel <- qa_ui_advice_panel(
    title = "Including inter-occasion variability",
    htmltools::tags$p(
      "The expected OFV improvement in the overview table will give you an indicator if IOV should be included in
      your model. The estimated standard deviation of the IOV will then help you to decide which of the parameters
      in the original model should receive IOV."),
    htmltools::tags$p("You can use the PsN transform tool to add IOV to your model automatically. You will need
    to specify which column in the data contains the occasion identifier and which parameters IOV should be added to.
    For example, to add IOV to the CL parameter when the occasion is identified by the visit data item,
    you would use the command:",
    ),
    PsNR::qa_ui_console("transform add_iov run1.mod -occ=VISIT -parameters=CL"),
    htmltools::tags$p("PsN will add IOV to all paramters with IIV if the -parameters option is not provided, i.e.,",
    PsNR::qa_ui_console("transform add_iov run1.mod -occ=VISIT")
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of adding interoccasion variability to the submitted model. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      advice_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_add_eta <- function(){


  intro_panel <- qa_ui_manual_panel(
    title = "Additional ETAs",
    htmltools::tags$p(
      "QA can investigate the benefits of including additional ETA variables in the model. The names of the parameter
      for which this should be done, need to be supplied using the command line option",
      htmltools::tags$code("-add_etas=PAR1,PAR2"),
      "QA will then evaluate how much the addition of IIV on these parameters improves the model fit. This evaluation
      will be accelerated using the linearized proxy model. "
    ),
    htmltools::tags$p(
      "The results of the evaluation are shown as the expected improvement in OFV as well as the estimated variability
      of old and new random effects. The variability estimates of the random effects before the inclusion of the additional
      ETAs are shown as a reference."
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Adding additional ETAs",
    htmltools::tags$p(
      "The dOFV can help you decide if the addition of ETAs is significantly improving the model fit. Furthermore,
      you should check that the estimated variability for the added ETA is sufficiently large. "),
    htmltools::tags$p("For the implementation, you can use the PsN transform tool to add ETA variables
    to your model automatically. You will only need to provide the name of the parameter(s), e.g.,",
    ),
    PsNR::qa_ui_console("transform add_eta run1.mod -parameters=CL"),
    htmltools::tags$p("will add an ETA to the parameter CL using a log-normal variability model. ")
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of adding additional ETA variables to the submitted model. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      advice_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_residual <- function(){


  intro_panel <- qa_ui_manual_panel(
    title = "Residual error model",
    htmltools::tags$p(
      "The residual error model is rarely the primary focus of an analysis, but it is of extreme significance as
      the main component deciding how much importance subsets of the data receive. In QA, a whole range of
      extended residual unexplained variability (RUV) models is evaluated automatically using the
      residual-based proxy model (see technical panel below). The different models test are "
    ),
    htmltools::tags$p(
      htmltools::tags$strong("IIV on RUV:"),
      "This model uses an additional random effect to allows for a subject-specific RUV magnitude."
    ),
    htmltools::tags$p(
      htmltools::tags$strong("Power"),
      "The power RUV model scales the magnitude of the residual error by a power of the model prediction. "
    ),
    htmltools::tags$p(
      htmltools::tags$strong("Time/TAD/IDV-varying RUV"),
      "This group of models tests a range of time, TAD and IDV cut-off values to estimate a separate RUV magnitude
      before and after the cut-off. TAD-varying models are automatically test if the TAD variable is present in $INPUT
      of the submitted model. An additional independent variable can be added using the ",
      htmltools::tags$code("-resmod_idv=VAR"),
      " command-line option. "

    ),
    htmltools::tags$p(
      htmltools::tags$strong("t-distributed RUV"),
      "This model uses a t-distributed residual error, which allows for heavier tails than the standard normal distribution."
    ),
    htmltools::tags$p(
      htmltools::tags$strong("Autocorrelation"),
      "This model relaxes the assumption of uncorrelated residuals and estimates a correlation half-life, i.e.,
      a time after which the correlation between residuals has decreased by a factor of two. This model is also known
      as AR1 error model. "
    ),
    htmltools::tags$p(
      htmltools::tags$strong("Dynamic transform both sides"),
      "In this model, a Box-Cox-like transformation is estimated to account for skewness and scedasticity of the residuals.
      It estimates two additional parameters, the shape parameter lambda and the power-term zeta."
    ),
    htmltools::tags$p(
      "A table below ranks the different RUV models based on the expected improvement. For the time/TAD varying models,
      only the cut-off value with the biggest improvement is shown. The overview table also shows the number of additional
      parameters as well as their estimated values. "
    )
  )

  technical_panel <- qa_ui_technical_panel(
    title = "CWRES-based RUV model building",
    htmltools::tags$p(
      "The investigation of the residual error model in QA is based on the residual-based model proxy. This proxy models
      the CWRES of the submitted model. Under the true model, the residuals are expected to be independent identically normally
      distributed with a mean of 0 and a variance of 1. Misspecifications in the submitted model will be visible as
      divergence of the CWRES from these characteristics. QA tests for these divergences in a hypothesis-driven manner by
      estimating different transformations and comparing the fit to the base model (assuming mean 0 variance 1). "
    ),
    htmltools::tags$p(
      "For example, to evaluate the 'IIV on RUV' model the proxy-model
      $${y}_i={\\theta}_1+{\\eta}_{1i}+\\varepsilon_{1i}\\cdot \\exp \\left({\\eta}_{2i}\\right)$$
      is fit to the CWRES of the submitted model and the OFV is compared to the OFV of the base proxy model, i.e.,
      $${y}_i={\\theta}_1+{\\eta}_{1i}+\\varepsilon_{1i}$$
      The difference in OFVs is the reported dOFV and the estimated variance for \\(\\eta_{2i}\\) is the additional parameter
      in the summary table."
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Selecting and implementing an RUV model",
    htmltools::tags$p(
      "The reported dOFV values, together with the cut-off values from the chi-square distribution, can help you to decide
      whether one of the extended RUV models is worth implementing. In addition to the statistical significance, it is also important
      to judge whether an improved description of the data justifies the added complexity."
    ),
    htmltools::tags$p(
      "The actual implementation will very much depend on the selected model. For the 'IIV on RUV' and power residual error
      models the PsN transform tool can automatically perform it for you The following command will add the
      'IIV on RUV' model to the supplied control stream ",
      PsNR::qa_ui_console("transform iiv_on_ruv run1.mod"),
      " and to add the power residual error model, you can use",
      PsNR::qa_ui_console("transform power_on_ruv run1.mod")
    ),
    htmltools::tags$p(
      "You can use the 'dynamic transform both sides' approach through the -dtbs command-line option of the PsN execute tool.
      Have a look at the ",
      htmltools::tags$a("user guide", href = "https://github.com/UUPharmacometrics/PsN/releases/download/4.9.0/execute_userguide.pdf"),
      " for execute to learn how."),
    htmltools::tags$p(
      " The remaining extended RUV models need to be implemented manually. In this case, it can be helpful to use the code
      generated by QA as a scaffold. ")

  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section shows the effect of including extended residual error models in the submitted model. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      technical_panel,
      advice_panel
    )
  )
}


#' @export
#' @rdname qa_report_content
qa_report_content_influential <- function(){


  intro_panel <- qa_ui_manual_panel(
    title = "Influential individuals",
    htmltools::tags$p(
      "Influential individuals are subjects in the data whose removal would noticeably change the results of the estimation.
      In QA, this is evaluated using case deletion diagnostics (CDD), which refit the model with one subject at a time removed
      from the analysis dataset. QA uses the linearized model proxy to accelerate this analysis (see subsequent panel for
      details). "
    ),
    htmltools::tags$p(
      "The results of the CDD are presented as a histogram showing the distribution of OFV improvements when excluding
      specific subjects during the fit. Furthermore, subjects with a value larger than 3.84 are reported in a table.
      The metric of improvement in OFV when excluding certain subjects is only intended as an indicator of magnitude
      and should not be interpreted as advice for removal (see the following advice panel)."
    )
  )

  technical_panel <- qa_ui_technical_panel(
    title = "Linearized dOFV-based case deletion diagnostics",
    htmltools::tags$p(
      "CDD traditionally utilize the Cook score as metrics for the individual influence on the parameter estimates.
      It requires the re-estimation of the NLME as well as the calculation of the covariance matrix for each of the
      individuals in the data. Using the delta-OFV (dOFV), to assess the influence of an individual on the quality of
      model fit to all other individuals avoids the covariance matrix and enables the use the linearized proxy model for
      further acceleration. In QA, the dOFV is calculated as
      $$dOFV=OFV_\\text{all}-iOFV_k-OFV_k$$
      where \\(OFV_\\text{all}\\) is the OFV of the full run with all individuals included, \\(iOFV_k\\) is the individual
      OFV of the k:th individual in the full run and \\(OFV_k\\) is the OFV of the run with the k:th individual removed.",
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Handling influential individuals",
    htmltools::tags$p(
      "Influential individuals are not an indication of a problem per se and sometimes are even expected (for example,
      if some subjects have more observations than the rest). However, care needs to be taken if an influential
      individual also shows up as an outlier in the subsequent section. This indicates that the outlying data drives
      the fit of the model and could indicate biased results."
    ),
    htmltools::tags$p(
      "Providing general advice for a situation of outlying, influential individuals is difficult. Sometimes the inclusion of
      additional covariates can help to improve the fit for these subjects. A residual error model that provides less weight
      to extreme observations or subject can help if no systematic explanation can be found (see  residuals sections for more
      advice). Excluding data from the analysis, however, should always be the last resort. "
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section investigates the presence of influential individuals. These are subjects that significantly
      influence the model fit for all other subjects. "
    ),
    qa_ui_tmode_content(
      intro_panel,
      technical_panel,
      advice_panel
    )
  )
}


#' @export
#' @rdname qa_report_content
qa_report_content_outliers <- function(){


  intro_panel <- qa_ui_manual_panel(
    title = "Outliers",
    htmltools::tags$p(
      "Outliers are parts of the data that significantly differ from the rest. In an NLME, outliers can be defined at
      several different levels. In QA, outlying observations and subjects are considered. Roughly speaking,
      both are identified by comparing different model-based statistics for the data with the same statistics for
      simulated data (where the model is true by construction). QA uses the individual OFV as well as
      EBEs on the subject level and CWRES on the observation level as statistics (see the following panel for details). "
    ),
    htmltools::tags$p(
      "The results of the outlier analysis are presented as a table of subjects that have been identified as outliers,
      together with the improvement in OFV, were these to be excluded from the analysis. Following the table, a graph
      indicates the distribution in
      this is followed by individual plots showing individual and population predictions together with the data for the
      outlying subjects. Finally, a table indicates
      "
    )
  )

  technical_panel <- qa_ui_technical_panel(
    title = "Simulation-evaluation based diagnostics for outlier identification",
    htmltools::tags$p(
      "",
    )
  )

  advice_panel <- qa_ui_advice_panel(
    title = "Handling outliers",
    htmltools::tags$p(
      "The interpretation and advice for outliers echos the influential individuals section. An outlier alone does not
      necessarily represent a problem, but it should be carefully scrutinized if it has too much influence on the model
      fit. Ideally, a systematic explanation can be found and included in the model to make this subject/observation be
      better described. Otherwise, the residual error model can help to reduce its importance (see residual error section).
      Removing the offending piece of data should be the last resort. "
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section investigates the presence of outliers with a worse than expected model fit. "
    ),
    qa_ui_tmode_content(
      intro_panel,
    #  technical_panel,
      advice_panel
    )
  )
}



#' @export
#' @rdname qa_report_content
qa_report_content_covariates <- function(){


  about_covar_panel <- qa_ui_manual_panel(
    title = "Covariates section",
    htmltools::tags$p(
      "The impact of the covariates provided to QA using the ",
      htmltools::tags$code("-continuous=COV"),
      " and ",
      htmltools::tags$code("-categorical=COV"),
      "command-line options is summarized in the following section (the section is empty if no covariates are provided).
      Within QA, all covariates are evaluated two-fold, using a univariate screen approach and jointly using full random
      effect modeling (FREM). Both methods use the linearized proxy model and perform the evaluation directly on the ETA
      variables. "
    ),
    htmltools::tags$p(
      "The univariate screening approach tests one covariate and one ETA variable at a time and, hence, does not take
      correlations between covariates or parameters into account. It, therefore, closely mimics what would occur during
      the first step of step-wise covariate modeling (SCM) and allows us to identify important parameter-covariate
      relationships quickly."
    ),
    htmltools::tags$p(
      "FREM, on the other hand, describes the relationships between covariates and parameters jointly and therefore
      takes correlations into account. "
    ),
    htmltools::tags$p(
      "The report starts with an overview table of listing all tested univariate relationships, the estimated covariate
      coefficient, and the resulting improvement in OFV. The table, furthermore, details the sum of univariate relationships
      and improvement in OFV achieved using FREM."
    )


  )

  cov_screen_technical_panel <- qa_ui_technical_panel(
    title = "Univariate covariate screening",
    htmltools::tags$p(
      "For the univariate covariate screening, continuous covariates are centered at their median, and for categorical
      covariates, the effect is parameterized as the change from the most common category. The covariates are then added,
      one at a time, additively on the ETAs of the linearized proxy model. This translates to an exponential parameterization
      in for parameters with a log-normal variability model in the submitted model. The covariate coefficients are then
      estimated together with the change in OFV from the base model. "
    )
  )

  frem_technical_panel <- qa_ui_technical_panel(
    title = "Full random effects covariate modeling",
    htmltools::tags$p(
      "In the full random effect covariate modeling (FREM) approach, covariates are treated as observed data points
      and are modeled as random effects. A full covariance matrix between random effects for parameters and
      covariates is estimated together with the other model components. Coefficients for covariate-parameter
      relations are obtained from the ratio of covariance between parameter and covariate variability to the
      covariate variance. For example, the FREM model to include the covariate AGE on the parameter CL would look as follows
      $$Cl_i=\\theta_1\\cdot exp(\\eta_{1i})$$
      $$y_{i,\\text{conc}}=f(Cl_i,\\ldots)+\\varepsilon_{1,ij}$$
      $$y_{i,\\text{age}}=\\theta_2+\\eta_{2i}+\\varepsilon_{2,ij}$$
      where \\(Cl_i\\) is the individual CL parameter, \\(y_{i,\\text{conc}}\\) is the individual predicted concentration
      (the details of the model have been omitted), and \\(y_{i,\\text{age}}\\) is the individual predicted AGE with
      mean \\(\\theta_2\\) and variance \\(\\omega_2^2\\) (the variance of \\(\\varepsilon_{2,ij}\\) is fixed to a small
      value). The covariate effect would then be described through the covariance between the random effect for CL and AGE, i.e.,
      \\(\\omega_{1,2}\\)."
    ),
    htmltools::tags$p(
      "One of the many benefits of the FREM approach is that all covariates of interest can be jointly included in the model.
      Correlated covariates are taken into account and do not render the model instable. Furthermore, it is possible to
      derive the impact of individual covariates from the joint model, and missing covariate values are handled implicitly."
    ),
    htmltools::tags$p(
      "QA uses the linearized proxy model to accelerate the FREM analysis. "
    )
  )

  htmltools::tags$div(
    htmltools::tags$p(
      "This section evaluates the impact of the supplied covariates. "
    ),
    qa_ui_tmode_content(
      about_covar_panel,
      cov_screen_technical_panel,
      frem_technical_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_vaplot <- function(){
  if(!knitr::is_html_output()) return(invisible())

  intro_panel <- qa_ui_manual_panel(
    title = "Variability attribution plot",
    htmltools::tags$p(
      "The variability attribution plot visualizes the impact of the different sources of variability on the prediction. It
      shows how the influence of different ETA variables, the residual error as well as covariates changes over time and TAD
      (if available). In QA, the variability attribution is generated for the submitted model (before the inclusion of covariates)
      as well as after the inclusion of all provided covariates (using the FREM methodology). Covariates that are already
      present in the submitted model are not included in the visualization. "
    )
  )

  htmltools::tags$div(
    qa_ui_tmode_content(
      intro_panel
    )
  )
}

#' @export
#' @rdname qa_report_content
qa_report_content_frem_plots <- function(){
  if(!knitr::is_html_output()) return(invisible())

  intro_panel <- qa_ui_manual_panel(
    title = "FREM plots",
    htmltools::tags$p(
      "QA generates several plots based on the FREM model that help to understand the sources and distribution of
      covariate effect sizes in the data. In total, three types of plots are generated for each ETA variable in the
      submitted model. "
    ),
    htmltools::tags$p(
      "The first set of plots show the effects of each covariate on a particular parameter. Each covariate is represented
      by two bars. For continuous covariates, the bars represent the estimated covariate effect for the 5th (blue) and 95th
      percentile (red) of the covariate distribution as well as its uncertainty. For categorical covariates, the bars
      compare the effect of one category to the remaining ones. In addition to the plot, the effect size and its uncertainty
      is also given in a table format."
    ),
    htmltools::tags$p(
      "The second set of plots illustrate the covariate effects on the individual level. The plots show the total covariate
      effects and its uncertainty for the individuals with the most extreme covariate combinations. In addition to the
      plot of effect sizes, the individuals and their covariate values are shown in a table. "
    )
    ,
    htmltools::tags$p(
      "The final set of plots visualize the unexplained variability for a parameter before and after inclusion of
      the covariates. The top bar shows the unexplained variability, together with its uncertainty, before the
      inclusion of any covariate. The bars below illustrate the reduction in unexplained variability when a particular
      covariate is included in the model and final bar at the bottom depicts the unexplained variability if all covariates
      were to be included.
      "
    )

  )

  htmltools::tags$div(
    qa_ui_tmode_content(
      intro_panel
    )
  )
}


#' @export
qa_report_element_info_ofv_table <- function(){
  PsNR::qa_ui_element_info(
    element_id = "ofv-table",
    text = "Comparison of the OFVs from original and linearized proxy-model (before and after estimation). Large differences
    indicate problems with the linearization and warrant caution when interpreting the result of the report."
  )
}

#' @export
qa_report_element_info_overview_table <- function(){
  PsNR::qa_ui_element_info(
    element_id = "overview-table",
    text = "Result overview table showing the top-level resuts from the individual sections."
  )
}