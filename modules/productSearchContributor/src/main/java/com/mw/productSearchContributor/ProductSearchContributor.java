package com.mw.productSearchContributor;

import com.liferay.commerce.product.constants.CPField;
import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;
import com.liferay.portal.kernel.search.BooleanClauseOccur;
import com.liferay.portal.kernel.search.SearchContext;
import com.liferay.portal.kernel.search.filter.BooleanFilter;
import com.liferay.portal.kernel.search.filter.Filter;
import com.liferay.portal.kernel.search.filter.TermFilter;
import com.liferay.portal.kernel.util.GetterUtil;
import com.liferay.portal.kernel.util.Validator;
import com.liferay.portal.search.spi.model.query.contributor.ModelPreFilterContributor;
import com.liferay.portal.search.spi.model.registrar.ModelSearchSettings;

import org.osgi.service.component.annotations.Component;

@Component(
		property = {
			"indexer.class.name=com.liferay.commerce.product.model.CPDefinition",
			"indexer.clauses.mandatory=true"
		},
		service = ModelPreFilterContributor.class
	)
	public class ProductSearchContributor
		implements ModelPreFilterContributor {
	
	
	
	@Override
	public void contribute(
		BooleanFilter booleanFilter, ModelSearchSettings modelSearchSettings,
		SearchContext searchContext) {
		
		_log.info("contribute");

		_filterByAccountGroupIds(booleanFilter, searchContext);

	}
	
	private void _filterByAccountGroupIds(
			BooleanFilter booleanFilter, SearchContext searchContext) {

			BooleanFilter accountGroupsBooleanFilter = new BooleanFilter();

			BooleanFilter accountGroupsFilterEnableBooleanFilter =
				new BooleanFilter();

			accountGroupsFilterEnableBooleanFilter.addTerm(
				CPField.ACCOUNT_GROUP_FILTER_ENABLED, Boolean.TRUE.toString(),
				BooleanClauseOccur.MUST);

			long userId = searchContext.getUserId();
			
			if (userId > 0) {
				_log.info("UserId: " + userId);
			}
			
			///boolean commerceAccountGroupIdsFilterRequired = .... searchContext.getAttribute("search.experiences.commerceAccountGroupIdsFilterRequired") // default to false
			
			//if commerceAccountGroupIdsFilterRequired is true and user > 0 then get that users commerceAccountGroupIds using java API and use below in place of the code in the TODO START / END below...
			if (userId > 0) {
				_log.info("UserId: " + userId);
			}
			
			// TODO START REPLACE with java API logic...
			String mwCommerceAccountGroupIds = GetterUtil.getString(searchContext.getAttribute("search.experiences.mwCommerceAccountGroupIds"), null);
			
			_log.info("mwCommerceAccountGroupIds: " + mwCommerceAccountGroupIds);
		
			if (Validator.isNull(mwCommerceAccountGroupIds)) return;
			
			String[] mwCommerceAccountGroupIdsArray = mwCommerceAccountGroupIds.split(",");
			// TODO END
			
			if ((mwCommerceAccountGroupIdsArray != null) && (mwCommerceAccountGroupIdsArray.length > 0)) {
				BooleanFilter accountGroupIdsBooleanFilter = new BooleanFilter();

				for (String accountGroupId : mwCommerceAccountGroupIdsArray) {
					Filter termFilter = new TermFilter(
						"commerceAccountGroupIds", String.valueOf(accountGroupId));

					accountGroupIdsBooleanFilter.add(
						termFilter, BooleanClauseOccur.SHOULD);
				}

				accountGroupsFilterEnableBooleanFilter.add(
					accountGroupIdsBooleanFilter, BooleanClauseOccur.MUST);
			}
			else {
				accountGroupsFilterEnableBooleanFilter.addTerm(
					"commerceAccountGroupIds", "-1", BooleanClauseOccur.MUST);
			}

			accountGroupsBooleanFilter.add(
				accountGroupsFilterEnableBooleanFilter, BooleanClauseOccur.SHOULD);
			accountGroupsBooleanFilter.addTerm(
				CPField.ACCOUNT_GROUP_FILTER_ENABLED, Boolean.FALSE.toString(),
				BooleanClauseOccur.SHOULD);

			boolean ignoreAccountGroup = GetterUtil.getBoolean(
				searchContext.getAttribute("ignoreCommerceAccountGroup"));

			if (!ignoreAccountGroup) {
				booleanFilter.add(
					accountGroupsBooleanFilter, BooleanClauseOccur.MUST);
			}
		}	
	
	private static final Log _log = LogFactoryUtil.getLog(
			ProductSearchContributor.class);	
}