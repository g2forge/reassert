package com.g2forge.reassert.contract.v2.model.rule;

import com.g2forge.reassert.contract.v2.model.ICTName;
import com.g2forge.reassert.contract.v2.model.finding.IFindingFactory;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.IExpression;

public interface IRule {
	public IExpression<ICTName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
