library(tidyverse)
library(ellmer)

system_prompt <- r"---(
You are an expert content analyst coding newspaper headlines about the Russia–Ukraine war.

TASK
For each headline, decide how the war is going for Ukraine *according to the headline*. 
Code the situation the headline describes or implies, not the tone of the outlet, 
and not what you know about the war from other sources. Use only the headline text.

CATEGORIES
- GOOD: The headline portrays developments favourable to Ukraine's war effort or position. 
  Examples: territorial gains or successful defence, Russian losses or setbacks, new or 
  increased military/financial aid, diplomatic support, sanctions hurting Russia, 
  Ukrainian morale or resilience emphasised.
- POOR: The headline portrays developments unfavourable to Ukraine. Examples: territorial 
  losses or retreats, Ukrainian military losses, Russian advances or successful strikes on 
  military/energy infrastructure, aid delayed/reduced/blocked, weakening Western support, 
  pressure on Ukraine to make concessions, manpower or ammunition shortages.
- MIXED: The headline explicitly contains both favourable and unfavourable elements and 
  neither clearly dominates.
- NONE: The headline is about the war but carries no implication about how it is going 
  for Ukraine (e.g. purely procedural, humanitarian or human-interest without a war-outcome 
  angle), or it is not about the war at all.

DECISION RULES
1. Ask: "Would a reader of this headline alone conclude things are going better or worse 
   for Ukraine?" Code that conclusion.
2. Civilian suffering caused by Russian attacks is POOR only if the headline frames it as 
   damaging Ukraine's capacity or position (e.g. energy grid collapse); otherwise NONE.
3. Statements and claims count as developments: "Zelensky says counteroffensive stalled" 
   is POOR; "Kremlin claims capture of village" is POOR unless the headline signals the 
   claim is disputed.
4. Questions and speculation ("Is Western support fading?") are coded by the scenario 
   they raise, unless the headline is clearly balanced.
5. When in doubt between GOOD/POOR and MIXED, choose MIXED only if both directions are 
   explicitly present in the text.

OUTPUT
Respond with only a JSON object:
{"label": "GOOD" | "POOR" | "MIXED" | "NONE", "reason": "<one short sentence>"}

EXAMPLES
Headline: "Ukrainian forces push Russians back across the Dnipro"
{"label": "GOOD", "reason": "Ukrainian territorial gain."}

Headline: "Avdiivka falls as Ukraine runs short of shells"
{"label": "POOR", "reason": "Territorial loss and ammunition shortage."}

Headline: "US Congress again delays vote on Ukraine aid package"
{"label": "POOR", "reason": "Military aid delayed."}

Headline: "Kyiv shoots down drones but strikes knock out power in Kharkiv"
{"label": "MIXED", "reason": "Successful air defence alongside damage to infrastructure."}

Headline: "Ukrainian refugees in Poland open bakery chain"
{"label": "NONE", "reason": "Human-interest story with no implication for the war's course."}

Headline: "EU agrees new sanctions package targeting Russian oil revenue"
{"label": "GOOD", "reason": "International pressure on Russia supports Ukraine."}
)---"


d <- tribble(
  ~id, ~text,
  1, "Ukraine war briefing: Europe to join negotiations, says Kushner, as Putin warns against peace keepers",
  2, "Trump won’t save Ukraine. But Marine Le Pen’s betrayal could be worse",
  3, "Putin takes hard line in US talks as Ukraine says Starlink disrupted again",
  4, "Ukraine captures first Indonesian prisoner of war amid surge in Russian recruitment"
)

answer_structure <- type_object(
  label = type_enum(values = c("GOOD", "POOR", "MIXED", "NONE"), 
                    description="Would a reader of this headline alone conclude things are going better or worse 
for Ukraine?"),
  reason     = type_string("Concisely explain why you coded the headline this way")
)


# will read OPENAI_API_KEY from your environment. 
# 1. file.edit("~/.Renviron")  or usethis::edit_r_environ(), add 
# 2. add a line OPENAI_API_KEY=sk-proj-....
# 3. save the file and restart R 
chat <- chat_openai(
  model         = "gpt-5-mini",   # or "gpt-5-nano"; check the current model list
  system_prompt = system_prompt
)
res <- d |>
  mutate(reply=parallel_chat_structured(chat, interpolate("{{text}}"), type = answer_structure)) |>
  unnest(reply, keep_empty=TRUE)


# Can also use a local ollama or a shared local server:
chat_gemma <- chat_openai_compatible(
  base_url      = "https://nebula.cs.vu.nl/api",
  credentials   = function() Sys.getenv("NEBULA_API_KEY"),
  model         = "FAST.gemma4:31b",
  system_prompt = system_prompt,
  params        = params(temperature = 0)
)

res_gemma <- d |>
  mutate(reply=parallel_chat_structured(chat_gemma, interpolate("{{text}}"), type = answer_structure)) |>
  unnest(reply, keep_empty=TRUE)

res_gemma

inner_join(res, res_gemma, join_by(id, text), suffix=c("_chat", "_gemma"))
