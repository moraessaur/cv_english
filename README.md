# 📄 CV Generator (LLM + RMarkdown + Google Sheets)

A modular system to generate highly tailored CVs and application content using:

- 📊 Structured data (Google Sheets)
- 🤖 LLMs (OpenAI)
- 🧠 Prompt engineering
- 🧾 RMarkdown + pagedown rendering

---

## 🚀 Overview

This project generates CVs dynamically by:

Google Sheets (source of truth)
        ↓
R pipeline (data + LLM transformations)
        ↓
Rendered workbook (standardized schema)
        ↓
RMarkdown (cv.rmd)
        ↓
HTML / PDF CV output

It supports:

- Job-specific CV tailoring
- LLM-enhanced bullet rewriting
- Structured skills stack generation
- Reusable prompt system
- Automated rendering + versioning

---

## 📁 Project Structure

.
├── data/
├── prompts/
├── R/
├── scripts/
├── renders/
└── README.md

---

## 🧠 Core Concepts

### Source of Truth
Google Sheets is the single source of truth for all CV data.

### Standardized Schema
All sections are transformed into:

section | title | loc | institution | start | end | description_1 → description_5 | in_resume

### LLM Layer
LLMs are used strictly for transformation (rewriting, condensing, selecting).

---

## ⚙️ Pipeline Flow

1. Download master sheet from Google Drive  
2. Generate structured entries  
3. Build render workbook  
4. Upload to Drive  
5. Render HTML/PDF CV  

---

## 🧩 Key Functions

- generate_cv_entries()
- generate_role_entries()
- generate_skills_stack_entries()
- generate_education_entries()
- render_cv_from_sheet()

---

## 📝 Prompt System

Located in:

prompts/

Supports:

- job descriptions
- recruiter messages
- skills stack prompts
- base formatting rules

---

## 🎨 Rendering

Handled via:

scripts/cv.rmd

Using pagedown::html_resume.

---

## ⚠️ Common Issues

- Blank HTML → missing section
- Layout breaking → too many bullets
- Render crash → empty section loop

---

## 🧠 Philosophy

Data → LLM → Render

Strict schema, modular prompts, reproducible pipeline.

---

## 👤 Author

Lucas Moraes  
Senior Data Scientist
