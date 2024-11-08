Note: This POC uses attribute name: search.experiences.mwCommerceAccountGroupIds

To test:

1. Build and deploy the custom module.

2. Create an empty Blueprint.

3. Create a Search Bar Widget Template:

- See sample widgetTemplate/widgetTemplate.txt and method getData(). This passes a custom attribute with hardcoded value. Must be a comma separated string...

For example:

"search.experiences.mwCommerceAccountGroupIds": "34615,34617"

where 34615 and 34617 are example account group Ids.

4. Update the Search Bar Configuration on the Search Page
- Use the Search Bar Widget Template from above.
- Use the Blueprint from above.

5. Setup test data and replace the hardcoded account group Ids.

6. Test

TODO:
- Add logic to get the account group Ids for the current user based on the currently selected account in the current site.