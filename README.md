📘 Gym Expert System — COM6008 Knowledge-Based Systems in AI

A Prolog-based expert system that provides gym training recommendations between sets, based on performance, fatigue indicators, sleep quality, soreness, recovery metrics, and training goals.

This project is part of the COM6008: Knowledge-Based Systems in AI coursework.

🚀 Features

✔ Expert knowledge encoded as ~24 rules
✔ Supports strength, hypertrophy, and endurance goals
✔ Provides recommendations for:
    •    Rest period (seconds)
    •    Load adjustment (+/− %)
    •    Volume modification
    •    Deload warnings
    •    Technique focus
    •    Warmup changes

🧠 How It Works

The expert system uses production rules (IF–THEN style) to make decisions based on:
    •    Performance: reps hit vs target, RPE, performance drop %
    •    Readiness indicators: soreness, sleep hours, sleep quality, HRV, subjective recovery
    •    Training goal: strength / hypertrophy / endurance
