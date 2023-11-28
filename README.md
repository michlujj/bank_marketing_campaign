Bank marketing

Context:

This dataset is related to direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact with the same client was required. The business problem is to predict whether the client would accept the campaign product (column y = “yes” or “no”).

Dataset: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

Business understanding:

What are the sociodemographics of the bank's customers?

To understand what are the predictors that will make customers sign up for bank marketing campaign

Dataframe understanding
Dataset contains 45211 observations and 17 variables. There are 7 numeric and 10 categorical columns.

age: age of client (numeric)

job: type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')

marital: marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)

education: (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')

default: has credit in default?

balance: average yearly balance

housing: has housing loan?

loan: has personal loan?

contact: contact communication type (categorical: 'cellular','telephone')

day_of_week: last contact day of the week

month: 	last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')

duration: last contact duration, in seconds (numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.

campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)

pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; -1 means client was not previously contacted)

previous: number of contacts performed before this campaign and for this client

poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')

y: 	has the client subscribed a term deposit?

