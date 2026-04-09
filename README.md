# paper_min_raters
R-based workflow to estimate the minimum number of raters required for validation of standard area diagrams (SADs), combining precision- and power-based statistical criteria.
---

## Overview

This repository provides a reproducible analytical framework in **R** to estimate the minimum number of raters required in validation studies of **standard area diagrams (SADs)** for plant disease severity assessment.

The proposed approach integrates two complementary statistical criteria:

- A **precision-based criterion**, ensuring stability of agreement estimates across raters  
- A **power-based criterion**, ensuring adequate statistical power to detect meaningful improvements when SADs are used  

By combining these criteria, the workflow supports **evidence-based decisions** on sample size in SAD validation studies.

---

## Background

Visual assessment of plant disease severity remains central to plant pathology, yet it is inherently subjective and prone to variability among raters. Standard area diagrams (SADs) are widely used to improve the accuracy and consistency of these estimates.  

Despite their widespread adoption, there is currently no standardized method to determine the number of raters required for SAD validation. Existing studies often rely on arbitrary or conventional sample sizes, which may lead to underpowered or inefficient experimental designs.

This repository implements a quantitative framework to address this limitation by explicitly linking:

- Rater variability  
- Expected improvement in agreement  
- Statistical precision and power  

---

## Methods

### Precision-Based Criterion

Agreement between rater estimates and reference values is quantified using **Lin’s concordance correlation coefficient (CCC)**. Variability among raters is summarized using the **coefficient of variation (CV)** of CCC values.

The minimum number of raters is obtained by imposing a target CV threshold, ensuring that the estimated agreement is sufficiently stable.

### Power-Based Criterion

To evaluate the ability to detect improvements due to SAD use, a **paired analysis** is performed comparing aided and unaided assessments.

- Agreement values are transformed using the **Fisher z-transformation**  
- A target effect size (e.g., ΔCCC = 0.10) is specified  
- The required number of raters is derived to achieve a desired statistical power (e.g., 0.80)  

### Final Recommendation

The final recommended number of raters is defined as:

k_final = max(k_precision, k_power)

## Data Requirements

Input data must be organized in **long format**, containing the following variables:

| Variable | Description |
|----------|-------------|
| `study`  | Study or pathosystem identifier |
| `rater`  | Rater identifier |
| `leaf`   | Image or specimen identifier |
| `actual` | Reference (true) disease severity |
| `unaided`| Visual estimate without SAD |
| `aided`  | Visual estimate with SAD |

---

## Implementation

Analyses are implemented in **R** using the following packages:

install.packages(c("tidyverse", "ggplot2"))

## The pipeline performs:

Computation of rater-specific CCC values
Estimation of CV-based minimum sample size
Power analysis for paired comparisons
Derivation of final recommended number of raters
Generation of graphical outputs
Outputs

## The workflow produces:

Tables summarizing:
Rater-specific agreement metrics
Precision-based sample size estimates
Power-based sample size estimates
Final recommended number of raters

## Figures including:
Power curves
Agreement distributions
Cross-study comparisons
Applications

## This framework is intended for:

Validation of standard area diagrams (SADs)
Experimental design in plant pathology
Studies involving visual disease assessment
Research on inter-rater agreement

## Associated Publication

Romero-Benavides, M. P., Bock, C. H., Tomáz, R. G., Nunes, W. C., & Del Ponte, E. M.
Balancing precision and power in determining the number of raters needed for standard area diagram validation studies.

(not available yet)

## Reproducibility

All analyses are fully reproducible. Users are encouraged to:

Adapt the workflow to their own datasets
Modify assumptions (e.g., CV threshold, target effect size, power)
Extend the framework to other agreement metrics if needed

## License

MIT.

## Acknowledgements

This work was developed at the Universidade Federal de Viçosa (UFV) with support from:

Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES)
Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq)


## Contact

Mary Paz Romero-Benavides
romerob.mp@gmail.com
