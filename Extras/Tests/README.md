# Logistic_Regression Workflow Tests



These tests were run at 2016-11-15 20:36:00



| id|name             |status   |time           |message               |
|--:|:----------------|:--------|:--------------|:---------------------|
|  1|LogisticTest1    |&#9989;  |5.324 seconds  |4 warnings            |
|  2|LogisticTest2    |&#9989;  |5.409 seconds  |4 warnings            |
|  3|LogisticTest3    |&#9989;  |5.558 seconds  |4 warnings            |
|  4|LogisticTest4    |&#9989;  |5.993 seconds  |3 warnings            |
|  5|LogisticTest5    |&#9989;  |10.163 seconds |6 warnings            |
|  6|LogisticXDFTest1 |&#x274C; |0.132 seconds  |1 error and 1 warning |
|  7|SampleTest       |&#9989;  |5.195 seconds  |4 warnings            |


## UI Test Checklist.

1. Check that configuration persists. This includes widgets, tabs, pages, accordions, and pretty much any other ui element that the user interacts with. The configuration window should always.
2. Check navigation. This includes checking if pages show up correctly when the Customize/Back buttons are clicked. It is also important to check that persistence holds under these operations.
3. The `Customize` button should appear only after the user has filled out the model name, target variable and predictors.
4. Clicking on the `Customize` button should take the user to the second page.
5. Checking `Use sampling weights ...` should bring up a dropdown to let the user select a weighting variable. This dropdown should be clearable. If the user checks this option, but does not select a weighting variable, then the workflow should display a configuration time error.
