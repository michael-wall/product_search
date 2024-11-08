package com.mw.productSearchContributor;

import com.liferay.account.manager.CurrentAccountEntryManager;
import com.liferay.account.model.AccountEntry;
import com.liferay.account.service.AccountGroupLocalService;
import com.liferay.commerce.product.constants.CPField;
import com.liferay.portal.kernel.exception.PortalException;
import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;
import com.liferay.portal.kernel.search.BooleanClauseOccur;
import com.liferay.portal.kernel.search.SearchContext;
import com.liferay.portal.kernel.search.filter.BooleanFilter;
import com.liferay.portal.kernel.search.filter.Filter;
import com.liferay.portal.kernel.search.filter.TermFilter;
import com.liferay.portal.kernel.util.GetterUtil;
import com.liferay.portal.search.spi.model.query.contributor.ModelPreFilterContributor;
import com.liferay.portal.search.spi.model.registrar.ModelSearchSettings;

import java.util.Arrays;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

// Based on class CPDefinitionModelPreFilterContributor
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
	
	private long[] getCommerceAccountGroupIds(SearchContext searchContext, long groupId) {
		
		try {
			long userId = searchContext.getUserId();

			if (userId > 0 && groupId > 0) {
				_log.info("UserId: " + userId + ", groupId: " + groupId);
				
				AccountEntry accountEntry = _currentAccountEntryManager.getCurrentAccountEntry(groupId, userId);
				
				if (accountEntry != null) {
					_log.info("accountEntryId: " + accountEntry.getAccountEntryId() + ", accountEntryName: " + accountEntry.getName());
				
					long[] accountGroupIds = _accountGroupLocalService.getAccountGroupIds(accountEntry.getAccountEntryId());
					
					_log.info("accountGroupIds: " + Arrays.toString(accountGroupIds));
					
					return accountGroupIds;
				}
			}
		} catch (PortalException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	private void _filterByAccountGroupIds(
			BooleanFilter booleanFilter, SearchContext searchContext) {
		
			boolean commerceAccountGroupIdsFilterRequired = GetterUtil.getBoolean(searchContext.getAttribute("search.experiences.commerceAccountGroupIdsFilterRequired"), false);
			long commerceAccountGroupIdsGroupId = GetterUtil.getLong(searchContext.getAttribute("search.experiences.commerceAccountGroupIdsGroupId"), -1);
			
			_log.info("commerceAccountGroupIdsFilterRequired: " + commerceAccountGroupIdsFilterRequired + ", groupId: " + commerceAccountGroupIdsGroupId);
			
			if (!commerceAccountGroupIdsFilterRequired) return; // Do nothing...
			if (commerceAccountGroupIdsGroupId == -1) return; // Do nothing...

			long[] commerceAccountGroupIds = getCommerceAccountGroupIds(searchContext, commerceAccountGroupIdsGroupId);
			
			BooleanFilter accountGroupsBooleanFilter = new BooleanFilter();

			BooleanFilter accountGroupsFilterEnableBooleanFilter =
				new BooleanFilter();

			accountGroupsFilterEnableBooleanFilter.addTerm(
				CPField.ACCOUNT_GROUP_FILTER_ENABLED, Boolean.TRUE.toString(),
				BooleanClauseOccur.MUST);
			
			if ((commerceAccountGroupIds != null) && (commerceAccountGroupIds.length > 0)) {
				BooleanFilter accountGroupIdsBooleanFilter = new BooleanFilter();

				for (long accountGroupId : commerceAccountGroupIds) {
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

	@Reference
	private  AccountGroupLocalService _accountGroupLocalService;		
	
	@Reference
	private CurrentAccountEntryManager _currentAccountEntryManager;	
		
	private static final Log _log = LogFactoryUtil.getLog(
			ProductSearchContributor.class);	
}