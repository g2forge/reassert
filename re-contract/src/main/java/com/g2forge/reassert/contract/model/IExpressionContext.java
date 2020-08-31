package com.g2forge.reassert.contract.model;

import java.util.Collection;

import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.expression.express.IExpression;

public interface IExpressionContext {
	public IExpression<TermRelation> getExpression();
	
	public Collection<ITerm> getInputs();

	public Collection<ITerm> getOutputs();
}
