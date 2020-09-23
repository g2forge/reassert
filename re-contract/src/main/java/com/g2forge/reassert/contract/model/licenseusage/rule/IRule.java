package com.g2forge.reassert.contract.model.licenseusage.rule;

import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.licenseusage.ICTName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

public interface IRule {
	public IExpression<ICTName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
