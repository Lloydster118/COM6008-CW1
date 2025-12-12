# 📘 Gym Expert System  
### COM6008 — Knowledge-Based Systems in AI

A **Prolog-based expert system** that provides gym training recommendations *between sets*, based on performance, fatigue, recovery, and training goals.

This project was developed as part of the **COM6008: Knowledge-Based Systems in AI** coursework at **Buckinghamshire New University**.

---

## 🚀 Features

- ✔ Expert knowledge encoded as **~24 symbolic rules**
- ✔ Supports **strength**, **hypertrophy**, and **endurance** goals
- ✔ Generates recommendations for:
  - Rest duration (seconds)
  - Load adjustment (increase / hold / decrease + %)
  - Volume modification
  - Deload warnings
  - Technique focus
  - Warm-up adjustments
- ✔ Includes **interactive mode** and **automated test cases**

---

## 🧠 How the System Works

The system uses **production rules (IF–THEN)** to reason about training decisions using three information groups:

### 🔹 Performance Metrics
- Reps achieved vs target
- Rate of Perceived Exertion (RPE)
- Performance drop-off between sets (%)

### 🔹 Recovery & Readiness Indicators
- Muscle soreness
- Sleep duration
- Sleep quality
- Heart Rate Variability (HRV)
- Subjective recovery state

### 🔹 Training Goal
- Strength
- Hypertrophy
- Endurance

Each advisory component is inferred independently and combined into a single structured recommendation.

---

## 🧩 System Design

- Knowledge represented symbolically using Prolog predicates
- Rule priority controlled via clause ordering and cut operators
- Default fallback rules ensure the system always returns advice
- Conflicting indicators (e.g. good performance but poor recovery) are handled through **separation of concerns**, not numerical optimisation

This approach prioritises **explainability and transparency** over statistical learning.

---

## 📁 Project Structure

```
Gym-Expert-System-COM6008-CW1/
├── src/
│   └── gym_expert.pl
├── tests/
│   └── test_cases.pl
├── README.md
```

---

## ▶️ How to Run

### 1️⃣ Install SWI-Prolog  
https://www.swi-prolog.org

### 2️⃣ Set the working directory
```prolog
working_directory(_, 'your local file directory').
```

### 3️⃣ Load the expert system
```prolog
[src/gym_expert].
```

### 4️⃣ Run a sample recommendation
```prolog
recommend(strength, 5, 5, 8, 10, mild, 7.5, ok, normal, medium, Advice).
```

### 5️⃣ Run automated tests
```prolog
[tests/test_cases].
run_tests.
```

---

## 🧪 Testing

The system includes an automated Prolog test harness that evaluates:

- Typical training scenarios  
- Boundary conditions (e.g. RPE thresholds)  
- Conflicting indicators  
- Extreme fatigue and deload cases  

All test cases execute successfully, demonstrating consistent rule behaviour.

---

## 🎓 Academic Context

This project demonstrates:

- Rule-based knowledge representation
- Deterministic inference and rule prioritisation
- Explainable AI decision-making
- Systematic testing and evaluation

---

## 👤 Authors

**Harry and Siyan**  
Buckinghamshire New University  
COM6008 — Knowledge-Based Systems in AI
