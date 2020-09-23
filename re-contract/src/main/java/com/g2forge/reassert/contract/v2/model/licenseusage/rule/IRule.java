package com.g2forge.reassert.contract.v2.model.licenseusage.rule;

import com.g2forge.reassert.contract.v2.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.v2.model.licenseusage.ICTName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.IExpression;

public interface IRule {
	public IExpression<ICTName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
