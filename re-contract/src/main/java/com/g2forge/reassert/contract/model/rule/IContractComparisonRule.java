package com.g2forge.reassert.contract.model.rule;

import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

public interface IContractComparisonRule {
	public IExpression<IContractComparisonName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
