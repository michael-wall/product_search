Note: This POC uses attribute:

"search.experiences.commerceAccountGroupIdsFilterRequired" = true;
"search.experiences.commerceAccountGroupIdsGroupId" = themeDisplay.getScopeGroupId();

To test:

1. Build and deploy the custom module.

- This uses the userId (from searchContext) and the groupId passed as an attribute to get the active AccountEntry then get the AccountGroupIds for that accountEntry.

2. Create an empty Blueprint.

3. Create a Search Bar Widget Template:

- See sample widgetTemplate_SearchContributorApproach_NO_HARDCODING.txt and method getData(). 

This passes 2 custom attributes:

"search.experiences.commerceAccountGroupIdsFilterRequired" = true;
"search.experiences.commerceAccountGroupIdsGroupId" = themeDisplay.getScopeGroupId();

4. Update the Search Bar Configuration on the Search Page
- Use the Search Bar Widget Template from above.
- Use the Blueprint from above.

5. Setup test data.

6. Test
