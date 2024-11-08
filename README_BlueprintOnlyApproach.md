>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Custom Element:
>>>>>>>>>>>>>>>>>>>>>>>>>>>>

{
	"description_i18n": {
		"en_US": "MW Commerce"
	},
	"elementDefinition": {
		"category": "custom",
		"configuration": {
			"queryConfiguration": {
				"queryEntries": [
					{
						"clauses": [
							{
								"context": "query",
								"occur": "filter",
								"query": {
									"terms": {
										"commerceAccountGroupIds": "${search.experiences.mwCommerceAccountGroupIds}"
									}
								}
							}
						]
					}
				]
			}
		},
		"icon": "custom-field"
	},
	"title_i18n": {
		"en_US": "MW Commerce"
	},
	"type": 0
}

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Blueprint > Parameter Configuration:
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

{
	"parameters": {
		"search.experiences.mwCommerceAccountGroupIds": {
			"type": "LongArray"
		}
	}
}

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Suggestions API call attributes:
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

"search.experiences.mwCommerceAccountGroupIds":"34615,34617"