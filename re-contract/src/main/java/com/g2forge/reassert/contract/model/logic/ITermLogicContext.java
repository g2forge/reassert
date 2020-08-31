package com.g2forge.reassert.contract.model.logic;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.expression.express.IExpression;

public interface ITermLogicContext {
	public default Object getContext() {
		throw new UnsupportedOperationException();
	}

	public IExpression<TermRelation> term(ILicenseTerm licenseTerm);

	public IExpression<TermRelation> term(IUsageTerm usageTerm);
}