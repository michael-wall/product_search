package com.mw.productSearchContributor;

import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;
import com.liferay.portal.kernel.search.BooleanQuery;
import com.liferay.portal.kernel.search.Field;
import com.liferay.portal.kernel.search.SearchContext;
import com.liferay.portal.kernel.util.GetterUtil;
import com.liferay.portal.search.query.QueryHelper;
import com.liferay.portal.search.spi.model.query.contributor.KeywordQueryContributor;
import com.liferay.portal.search.spi.model.query.contributor.helper.KeywordQueryContributorHelper;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

// Based on class CPDefinitionKeywordQueryContributor
@Component(
		property = {
			"indexer.class.name=com.liferay.commerce.product.model.CPDefinition"
		},
		service = KeywordQueryContributor.class
	)
	public class ProductSearchKeywordContributor
	implements KeywordQueryContributor {

	@Override
	public void contribute(
		String keywords, BooleanQuery booleanQuery,
		KeywordQueryContributorHelper keywordQueryContributorHelper) {

		SearchContext searchContext =
			keywordQueryContributorHelper.getSearchContext();
		
		_log.info("contribute");
		
		boolean commerceAccountGroupIdsFilterRequired = GetterUtil.getBoolean(searchContext.getAttribute("search.experiences.commerceAccountGroupIdsFilterRequired"), false);

		if (!commerceAccountGroupIdsFilterRequired) return; // Do nothing...
		
		_queryHelper.addSearchTerm(
			booleanQuery, searchContext, Field.NAME, false);
		_queryHelper.addSearchLocalizedTerm(
			booleanQuery, searchContext, Field.NAME, false);
	}

	@Reference
	private QueryHelper _queryHelper;
		
	private static final Log _log = LogFactoryUtil.getLog(
			ProductSearchKeywordContributor.class);	
}