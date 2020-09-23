package com.g2forge.reassert.contract.algorithm.usagepropagation.model;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.EdgeAccessorUsagePropagationName;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.IUsagePropagationName;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.TermUsagePropagationName;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.IVariable;
import com.g2forge.reassert.express.model.variable.Variable;

public interface IUsagePropagationBuilder<Term extends IUsageTerm, Edge extends IEdge> extends IBuilder<IFunction2<Edge, IUsage, IUsage>> {
	public IUsagePropagationBuilder<Term, Edge> compute(Term term, IExpression<IUsagePropagationName<Term, Edge>, TermRelation> expression);

	public default IUsagePropagationBuilder<Term, Edge> copy(Term term) {
		return compute(term, of(term));
	}

	public default IUsagePropagationBuilder<Term, Edge> exclude(Term term) {
		return set(term, TermRelation.Excluded);
	}

	public default IUsagePropagationBuilder<Term, Edge> include(Term term) {
		return set(term, TermRelation.Included);
	}

	public default <T> IVariable<IUsagePropagationName<Term, Edge>, TermRelation> of(ISerializableFunction1<? super Edge, ? extends T> accessor, IFunction1<? super T, ? extends TermRelation> adapter) {
		return new Variable<>(new EdgeAccessorUsagePropagationName<>(accessor, adapter));
	}

	public default IVariable<IUsagePropagationName<Term, Edge>, TermRelation> of(Term term) {
		return new Variable<>(new TermUsagePropagationName<>(term));
	}

	public default IUsagePropagationBuilder<Term, Edge> set(Term term, TermRelation relation) {
		return compute(term, new Literal<>(relation));
	}
}
