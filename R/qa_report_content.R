#' Content sections for the QA report
#' 
#' These function generate the different content section for the QA report
#' 
#' @return Latex/HTML for the section
#' @name  qa_report_content
#' @export
qa_report_content_intro <- function(){
  about_report_panel <- qa_ui_manual_panel(
    title = "The QA report", 
    htmltools::tags$p(
      "The aim of this report is to be simultaneously concise and intelligible. To achieve it allows 
      you to switch between two modes, Result Mode and Tutorial Mode. The former is focused on the results 
      and is intended for users with some experience with QA. The Tutorial Mode adds additional text to 
      the different sections, intended to explain what is shown in the report and how it should be 
      interpreted. The display of the results is identical between the different modes. Currently, 
      the report is in Tutorial Mode and you can use the button in the navigation bar to return to 
      the Result Mode."
   ),
   htmltools::tags$p(
     "In addition to switching between modes, the navigation bar also allows you to directly jump to a 
     specific section. A click on the \"PsN QA\" link will bring you back to the top. "
   ),
   htmltools::tags$p(
     "In Tutorial Mode, the report is structured using three different panels, distinguishable by icon:",
     qa_ui_icon("glyphicon-user"),
     " indicates information that provides an introduction to QA as one would typically find in a user guide, ",
     qa_ui_icon("glyphicon-cog"),
     " indicates technical details that are not strictly necessary to use QA, and ",
     qa_ui_icon("glyphicon-flash"),
     " indicates actionable advice that help to advance the model buiding process. Throughout the report you 
     will also find the ",
      qa_ui_icon("glyphicon-info-sign"),
     " symbol next to figures and tables. Clicking on it will reveal additional explanations 
     for this specific element. Clicking the symbol again will hide the information."
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
      proxy-models to perform the evaluation (see following panel for details)."
    ),
    htmltools::tags$p(
      "The expected impact of the model modifications on the fit is quantified in terms of OFV improvement (dOFV). 
      The larger the expected change in OFV the more the model is expected to improve when this modification 
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
      The residual-based model is also extremly stable and fast.", 
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
      "The overview section provides a high-level summary of the results obtained. For each of the 
      six model aspects, the most impactful modifications are listed together with the expected 
      improvement in ",
      htmltools::tags$abbr(title="objective function value", "OFV"),
      " as well as the number of parameters requried to implement them. Large improvements warrant 
      further investigation in the details sections of the report. Low values throught this table 
      indicate a well performing model."
    )
  )
  
  htmltools::tags$div(
    qa_ui_tmode_content(
      about_overview_panel
    ),
    htmltools::tags$p(
      "The following overview table lists the expected improvement in OFV for the most impactful 
      model modifications as well as the required number of additional parameters."
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
      "(TIME and PRED are always generated, TAD if the variable is available, and an additional 
      one can be added using the command line option ",
      htmltools::tags$code("-resmod_idv=VAR"), 
      ". For each IDV separately, QA splits the data into bins with equal amount of observations.
      It than uses the residuals of the submitted model to estimate the difference between 
      observations and model predictions (see technical section to understand how). The impact 
      of the estimated difference is presented using two complementary measures: difference in 
      OFV and the mean bias per bin. The former is an estimate of how much the OFV of the 
      submitted model could improve if the bias is addressed and the latter quantifies the bias 
      on the same scale as the observations."
    ),
    htmltools::tags$p(
      "  This report contains a subsection for each IDV that was used, with an identical layout 
      within each subsection. First, a table indicates what type of residuals and what IDV 
      were used, and what the expected OFV improvment is. The following table lists the bias 
      on the residual as well as on the observation scale for each bin. The observation scale 
      is indicated as ",
      htmltools::tags$abbr(title="conditional PRED", "CPRED"),
      "(following NONMEM terminology). The first plot in each subsection visualizes the bias, 
      on the observations scale, for each bin and, hence, allows an easier identification of 
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
      for substanial bias. The plots of estimated bias vs. independent variable can than help to 
      identify where the misspecification lies. If for example, bias is largest for early time 
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
      independent variables.The following overview table lists the expected improvement in OFV for the 
      most impactful."
    ),
    qa_ui_tmode_content(
      about_structural_panel,
      resmod_panel,
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
      (1) addition of a full OMEGA block, (2) Box-Cox transformation of the random effects, (3) inclusion of additional 
      ETA variables, (4) t-distribution transformation, and (5) inclusion of inter-occasion variability. A dedicated sections 
      details the results of applying these modifications. Sometimes, a modification might be skipped if it was not requested
      or is not applicable, the corresponding subsection will then detail why no results were obtained. "
    )
  )
  
  parvar_technical_panel <- qa_ui_technical_panel(
    title = "Linearization-based parameter variability model building",
    htmltools::tags$p(
      "All modifications presented in this section use the linearized proxy model as a basis. Their implementations 
      are identical to the non-linear case. Nonetheless, differences between the expected OFV improvement and the actual 
      one are     ",
      qa_ui_cite("svensson2014")
    )
  )
  
  htmltools::tags$div(
    htmltools::tags$p(
      "This section evaluates the variability model of the parameters in the submitted model with respect to a number of 
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
qa_report_content_covariates <- function(){
  about_covar_panel <- qa_ui_manual_panel(
    title = "Covariates section", 
    htmltools::tags$p(
      "The impact of the covariates provided to QA using the ",
      htmltools::tags$code("-continuous=COV"),
      " and ",
      htmltools::tags$code("-categorical=COV"),
      "command line options is summarized in the following section (the section is empty if no covariates are provided). 
      Within QA, all covariates are evaluated two-fold, using a univariate screen approach and jointly using full random 
      effect modeling (FREM). Both methods use the linearized proxy model and perform the evaluation directly on the ETA
      variables. "
    ),
    htmltools::tags$p(
      "The univariate screening approach tests one covariate and one ETA variable at a time and, hence, does not take 
      correlations between covariates or parameters into account. It, therefore, closely mimics what would occur during 
      the first step of step-wise covariate modeling (SCM) and allows to quickly identify important parameter-covariate
      relationships."
    ),
    htmltools::tags$p(
      "FREM on the other hand, describes the relationships between covariates and parameters jointly and therefore 
      takes correlations into account. "
    ),
    htmltools::tags$p(
      "The report starts with an overview table of listing all tested univariate relationships, the estimated covariate 
      coefficient and the resulting improvement in OFV. The table, furthermore, details the sum of univariate relationships
      and improvement in OFV achieved using FREM."
    )
    

  )
  
  cov_screen_technical_panel <- qa_ui_technical_panel(
    title = "Univariate covariate screening",
    htmltools::tags$p(
      "",
      qa_ui_cite("svensson2014")
    )
  )
  
  frem_technical_panel <- qa_ui_technical_panel(
    title = "Full random effects covariate modeling",
    htmltools::tags$p(
      "",
      qa_ui_cite("svensson2014")
    )
  )
  
  htmltools::tags$div(
    htmltools::tags$p(
      "This section evaluates the impact of the supplied covariates . "
    ),
    qa_ui_tmode_content(
      about_covar_panel,
      cov_screen_technical_panel,
      frem_technical_panel
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