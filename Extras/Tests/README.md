# Logistic_Regression Workflow Tests



These tests were run at 2017-01-07 03:44:55



| id|name          |status   |time           |message                 |
|--:|:-------------|:--------|:--------------|:-----------------------|
|  1|LogisticTest1 |&#9989;  |9.014 seconds  |2 warnings              |
|  2|LogisticTest2 |&#9989;  |8.913 seconds  |2 warnings              |
|  3|LogisticTest3 |&#9989;  |8.136 seconds  |2 warnings              |
|  4|LogisticTest4 |&#9989;  |9.793 seconds  |1 warning               |
|  5|LogisticTest5 |&#9989;  |13.961 seconds |4 warnings              |
|  6|LogisticTest6 |&#x274C; |7.332 seconds  |3 errors and 3 warnings |
|  7|SampleTest    |&#9989;  |9.118 seconds  |2 warnings              |


## UI Test Checklist.

1. Check that configuration persists. This includes widgets, tabs, pages, accordions, and pretty much any other ui element that the user interacts with. The configuration window should always.
2. Check navigation. This includes checking if pages show up correctly when the Customize/Back buttons are clicked. It is also important to check that persistence holds under these operations.
3. The `Customize` button should appear only after the user has filled out the model name, target variable and predictors.
4. Clicking on the `Customize` button should take the user to the second page.
5. Checking `Use sampling weights ...` should bring up a dropdown to let the user select a weighting variable. This dropdown should be clearable. If the user checks this option, but does not select a weighting variable, then the workflow should display a configuration time error.
