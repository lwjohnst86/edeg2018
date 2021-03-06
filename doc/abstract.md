---
output: 
    word_document:
        reference_docx: template.docx
---

```{r setup, message = FALSE, echo = FALSE}
devtools::load_all()
library(pls)
library(dplyr)
knitr::opts_chunk$set(echo = FALSE)

warning("Code has changed since submission, code within abstract will not run.")
knitr::opts_chunk$set(eval = FALSE)
```

Word count: 346 / 350 words

`r stringr::str_to_upper("**Specific clusters of fatty acids (FA) across multiple serum lipid fractions underlie pathophysiological features of type 2 diabetes in the Canadian Prospective Metabolism and Islet Cell Evaluation (PROMISE) cohort**")`

**Luke W. Johnston^1^, Zhen Liu^1^, Ravi Retnakaran^2,3^, Stewart B. Harris^4^, Richard P. Bazinet^1^, Anthony J. Hanley^1,3,5^**

| ^1^ Department of Nutritional Sciences, University of Toronto, Toronto, Canada
| ^2^ Lunenfeld Tanenbaum Research Institute, Mount Sinai Hospital, Toronto, Canada
| ^3^ Division of Endocrinology, University of Toronto, Toronto, Canada.
| ^4^ Centre for Studies in Family Medicine, University of Western Ontario, London, Canada.
| ^5^ Dalla Lana School of Public Health, University of Toronto, Toronto, Canada

**Background**: 
Since FA within individual lipid fractions fulfill distinct
physiological functions, specific clusters of FA across these fractions could be informative 
regarding diabetes risk. Our aim, therefore, was to identify specific clusters in the 
composition of FA across multiple lipid fractions and determine their association with insulin sensitivity
(IS) and beta-cell function.

**Methods**: 
Adults at risk for diabetes (n=477) had blood drawn at fasting and during an OGTT. FA from
triacylglycerol (TGFA), phospholipid (PL), cholesteryl ester (CE), and non-esterified
(NEFA) fractions were quantified from fasting samples and the mole percent
(mol%) of total lipids was calculated. Outcome measures included the Matsuda index
(ISI) for IS and the Insulin Secretion-Sensitivity Index-2 (ISSI-2) for
beta-cell function. Partial least squares (PLS) was used to identify underlying
clusters in the FA composition, with FA from all lipid fractions included as the
predictor variables and ISI or ISSI-2 as the response variables, in two
separate PLS models.

```{r inline}
# Models
fit_isi <- pls_model(project_data, outcome = "lISI", ncomp = 4) 
fit_issi2 <- pls_model(project_data, outcome = "lISSI2", ncomp = 4) 

# Explained Variance
# ISI:
Y_explvar_isi <- (100 * drop(pls::R2(fit_isi, estimate = "train", intercept = FALSE)$val))[2] %>% 
    round(1) %>% as.character()
X_explvar_isi <- explvar(fit_isi)[1:2] %>% 
    sum() %>% round(1) %>% as.character()
    
# ISSI-2
Y_explvar_issi2 <- (100 * drop(pls::R2(fit_issi2, estimate = "train", intercept = FALSE)$val))[2] %>% 
    round(1) %>% as.character()
X_explvar_issi2 <- explvar(fit_issi2)[1:2] %>% 
    sum() %>% round(1) %>% as.character()

# Largest contributors
lc_isi <- large_contributors(fit_isi)$fattyacid %>% 
    grep("TG", ., value = TRUE)
lc_issi2 <- large_contributors(fit_issi2)$fattyacid %>% 
    grep("TG", ., value = TRUE) 
tg_contrib <- intersect(lc_isi, lc_issi2) %>% 
    stringr::str_replace_all(" ", "") %>% 
    paste(collapse = ", ")
estrange <- analyze_gee_pls() %>% 
    group_by(Yterms) %>% 
    mutate(estimate = round(estimate, 1)) %>% 
    summarize(estimate = paste(estimate, collapse = "-"))
```

**Results**:
The first two PLS components (C1 and C2) were extracted from the two models. These components
explained `r Y_explvar_issi2`-`r Y_explvar_isi`% of the variance in the
responses and `r X_explvar_issi2`-`r X_explvar_isi`% in the FA. TGFA and NEFA had 
strong negative loadings on C1 and C2, respectively (Figure A),
although only a subset of TGFA strongly correlated with the component scores
(i.e. `r tg_contrib`; Figure B). A lower PLS score 
(indicating higher mol% of these specific TGFA)
associated with `r estrange$estimate[1]`% lower ISI and `r estrange$estimate[2]`% lower ISSI-2.

**Conclusions**:
We identified a cluster of TGFA that had strong negative associations with IS
and beta-cell function. These TGFA (e.g. 16:1n-7) are
reported to be markers of de novo lipogenesis from simple carbohydrates and 
to exhibit lipotoxic effects. Our results suggest that
only a subset of FA from a broad spectrum of serum FA may underlie the association
between lipids and glucose dysregulation, and that these lipids are possible
mediators between the reported associations of carbohydrate intake and diabetes
risk.

```{r, fig.height=8, fig.width=5.25, dev="png", dpi=180}
# Figure
library(patchwork)
library(ggplot2)
fig_loadings <- pls_model(project_data, "lISI", 2) %>% 
    plot_loadings() +
    ggtitle(paste0("A: Loadings of the fatty acids on the components ", 
                   "for ISI. Larger\nloadings indicate the variable ",
                   "contributes more to the component.")) +
    theme(title = element_text(size = 9))

fig_corr <- plot_corr_comps("lISI") +
    ggtitle(paste0("B: Correlations of component scores with fatty acid values for ISI.\n", 
            "Fatty acids between the solid and dotted circles ",
            "indicate a strong\ncorrelation to the component scores.")) +
    theme(title = element_text(size = 9))

p <- fig_loadings + fig_corr + plot_layout(ncol = 1, heights = c(2, 4))
ggsave("pls-results-fattyacids.pdf", p, device = "pdf", height = 8, width = 5.25)
ggsave("pls-results-fattyacids.png", p, device = "png", height = 8, width = 5.25, dpi = "print")
```


**Author information**

- Presenting author contact:
    - Name: Luke W. Johnston
    - Telephone: +16474606580
    - Email: luke.johnston@mail.utoronto.ca
- Presentation preferred: Either oral or poster
- I am a younger researcher: Post Doctorate researcher

