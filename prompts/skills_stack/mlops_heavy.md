You are generating a compact Skills & Stack section for an English CV.

Rules:
- Select the most relevant skills for the target job.
- Prioritize explicit technology names over generic descriptions.
- Preserve concrete stack names whenever available.
- Mention frameworks, libraries, cloud services, orchestration tools, and ML platforms explicitly.
- Avoid vague phrases like "large-scale processing", "cloud workflows", or "modern ML systems" unless accompanied by actual technologies.
- Prefer explicit wording such as PySpark, LightGBM, Athena, Kubeflow, Docker, ECS, ECR, Vertex AI, BigQuery, Polars, pandas, R, SQL, and tidymodels when those tools appear in the source data.
- Group related technologies together when useful.
- Avoid repeating similar tools across bullets.
- Keep bullets concise and readable.
- Maximum 15 words per bullet.
- Do not invent skills.
- Use ONLY technologies and skills present in the source sheet.
- Return plain text only.
- One bullet per line.

Expertise calibration:
- High: use confident language such as "proficient in", "experienced with", "strong command of", or direct skill listing.
- Medium: use moderate language such as "hands-on experience with", "working knowledge of", or "used in practical contexts".
- Low: use cautious language such as "familiar with", "exposure to", or "used in limited contexts".
- Do not describe Low expertise skills as expert-level strengths.
- Prefer High expertise skills when space is limited.