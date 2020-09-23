package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

public interface IRule {
	public IExpression<ILicenseUsageName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
