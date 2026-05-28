

# rapsimng.decide.sowing

`rapsimng.decide.sowing` provides functions to analyse APSIM Next Generation simulation outputs to support sowing decision under defined environments and management assumptions.

The package operates on APSIM outputs already loaded into R and focuses on transparent, reproducible evaluation of sowing window, establishment risk, and early crop development. It does not run APSIM simulations, interpret user intent, or make automatic recommendations.


---

## Installation


Currently on [Github](https://github.com/byzheng/rapsimng.decide.sowing) only. Install with:

```r
remotes::install_github('byzheng/rapsimng.decide.sowing')
```

---

## Overview

Farmers and advisors often ask:


> *What is the optimal sowing window and establishment risk for my paddock?*


This package supports that question by analysing long-term APSIM simulations to describe:

- optimal sowing window  
- establishment risk and early crop development  
- yield and risk across sowing dates  
- trade-offs between early and late sowing  

Instead of returning a single “best” date, the package provides a **decision landscape** that highlights strengths, weaknesses, and uncertainties for each sowing option.

---

## Scope

- APSIM **Next Generation** outputs only  
- Input: a tidy `data.frame` of simulation results  

- Analysis at **sowing date × environment × management** level  
- Deterministic, reproducible (no optimisation or AI)

---

## Not in Scope

- Running or modifying APSIM (`rapsimng`)
- Interpreting user intent (`agrillm`)
- Economic optimisation or automatic recommendation
- Black-box ranking or hidden decision rules

---

## Input Data

The package expects a `data.frame` (or `tibble`) where each row represents a simulation outcome for:


- sowing date (or sowing window factor)  
- year  
- environment/management factors  

Typical required variables:


- sowing date identifier  
- year  
- yield  
- establishment success/failure  
- optional: phenology stages, frost/heat indicators  

The data can come from any source (e.g. `rapsimng`, database export, CSV), as long as structure is consistent.

---

## Decision Context

The package assumes a decision context such as:


- fixed location/environment (e.g. Wagga Wagga)  
- defined sowing window (e.g. 1–15 May)  
- long-term climate variability (e.g. 30 years)  

---

## What the Package Provides


### 1. Sowing Window Performance

- mean / median yield by sowing date  
- establishment risk by sowing date  
- interannual variability  

---


### 2. Risk Assessment (optional)

- establishment failure risk  
- frost/heat exposure during early development  
- failure risk (probability below threshold yield)  

All risk definitions are explicit and stored as metadata.

---


### 3. Sowing Window Robustness

- performance across sowing dates within a window  
- sensitivity to sowing timing  

---


### 4. Trade-off Analysis

The package highlights trade-offs such as:

- early vs late sowing  
- high yield vs high risk  
- stable vs variable performance  

---

## Outputs

### Core Outputs


- sowing window performance summary  
- risk metrics per sowing date/window  
- assumption metadata  

---

### Decision-Support Outputs

- filtered candidates meeting criteria  
- explicit reasons for inclusion/exclusion  
- trade-off summaries  


Example:

> Sowing in early May has high mean yield but high establishment risk in dry years.  
> Sowing in late May has lower yield but more stable establishment and minimal frost exposure.

---

### Visual Outputs (optional)


- yield distributions by sowing date/window  
- yield vs risk trade-off plots  
- sowing window performance comparisons  

---

## Design Philosophy

This package follows three principles:

1. **Transparency**  
   All assumptions, thresholds, and metrics are explicit.

2. **Reproducibility**  
   Same input data always produces the same results.

3. **Separation of concerns**  
   - `rapsimng` → APSIM interaction  
   - `rapsimng.decide.*` → decision analysis  
   - `agrillm` → intent and explanation  

---

## Relationship to Other Packages

- **Depends conceptually on**: `rapsimng`  
- **Independent of**: APSIM runtime  
- **Callable by**: `agrillm`

---

## Key Insight


> This package does not select a sowing date.  
> It explains how sowing windows perform and trade off under uncertainty.

---

## Status

Early-stage design focused on:

- sowing window suitability under environment and management  
- extensibility to other decision domains  

---