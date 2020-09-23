package com.g2forge.reassert.contract.algorithm.usagepropagation.model;

import java.util.LinkedHashMap;
import java.util.Map;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.alexandria.java.type.function.TypeSwitch1.FunctionBuilder;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.EdgeAccessorUsagePropagationName;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.IUsagePropagationName;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.TermUsagePropagationName;
import com.g2forge.reassert.contract.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.eval.TermRelationValueSystem;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.PropagatedUsage;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.ValueEvaluator;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.environment.ATypeSwitchEnvironment;
import com.g2forge.reassert.express.model.variable.Closure;
import com.g2forge.reassert.express.model.variable.IVariable;
import com.g2forge.reassert.express.model.variable.Variable;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

public class UsagePropagationBuilder<Term extends IUsageTerm, Edge extends IEdge> implements IBuilder<IFunction2<Edge, IUsage, IUsage>> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	@EqualsAndHashCode(callSuper = false)
	@ToString(callSuper = false)
	protected static class EdgeTermEnvironment<Term extends IUsageTerm, Edge extends IEdge> extends ATypeSwitchEnvironment<IUsagePropagationName<Term, Edge>, TermRelation> {
		protected final Edge edge;

		protected final IUsage usage;

		protected <T> TermRelation apply(EdgeAccessorUsagePropagationName<Term, Edge, T> accessor) {
			final T value = accessor.getAccessor().apply(getEdge());
			return accessor.getAdapter().apply(value);
		}

		@Override
		protected void with(FunctionBuilder<IUsagePropagationName<Term, Edge>, IOptional<? extends IExpression<IUsagePropagationName<Term, Edge>, TermRelation>>> builder) {
			builder.add(EdgeAccessorUsagePropagationName.class, name -> {
				@SuppressWarnings("unchecked")
				final EdgeAccessorUsagePropagationName<Term, Edge, ?> cast = (EdgeAccessorUsagePropagationName<Term, Edge, ?>) name;
				return of(apply(cast));
			});
			builder.add(TermUsagePropagationName.class, name -> {
				@SuppressWarnings("unchecked")
				final TermUsagePropagationName<Term, Edge, ?> cast = (TermUsagePropagationName<Term, Edge, ?>) name;
				return of(getUsage().getTerms().getRelation(cast.getTerm()));
			});
		}
	}

	@RequiredArgsConstructor
	protected static class Function<Term extends IUsageTerm, Edge extends IEdge> implements IFunction2<Edge, IUsage, IUsage> {
		protected final Map<Term, IFunction2<Edge, IUsage, TermRelation>> map;

		@Override
		public IUsage apply(Edge edge, IUsage usage) {
			final Terms.TermsBuilder<IUsageTerm> termsBuilder = Terms.builder();
			for (Term term : map.keySet()) {
				final IFunction2<Edge, IUsage, TermRelation> function = map.get(term);
				final TermRelation relation = function.apply(edge, usage);
				termsBuilder.term(term, relation);
			}

			final PropagatedUsage retVal = new PropagatedUsage(edge, usage, termsBuilder.build());
			if (IUsage.isEqualTerms(usage, retVal)) return usage;
			return retVal;
		}

	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IEvaluator<IUsagePropagationName<Term, Edge>, TermRelation, TermRelation> evaluator = new ValueEvaluator<IUsagePropagationName<Term, Edge>, TermRelation>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());

	protected final Map<Term, IFunction2<Edge, IUsage, TermRelation>> map = new LinkedHashMap<>();

	@Override
	public IFunction2<Edge, IUsage, IUsage> build() {
		return new Function<>(new LinkedHashMap<>(map));
	}

	public UsagePropagationBuilder<Term, Edge> compute(Term term, IExpression<IUsagePropagationName<Term, Edge>, TermRelation> expression) {
		map.put(term, (edge, usage) -> {
			final IExpression<IUsagePropagationName<Term, Edge>, TermRelation> closure = new Closure<>(new EdgeTermEnvironment<>(edge, usage), expression);
			return getEvaluator().eval(closure);
		});
		return this;
	}

	public UsagePropagationBuilder<Term, Edge> copy(Term term) {
		map.put(term, (e, u) -> u.getTerms().getRelation(term));
		return this;
	}

	public UsagePropagationBuilder<Term, Edge> exclude(Term term) {
		return set(term, TermRelation.Excluded);
	}

	public UsagePropagationBuilder<Term, Edge> include(Term term) {
		return set(term, TermRelation.Included);
	}

	public <T> IVariable<IUsagePropagationName<Term, Edge>, TermRelation> of(ISerializableFunction1<? super Edge, ? extends T> accessor, IFunction1<? super T, ? extends TermRelation> adapter) {
		return new Variable<>(new EdgeAccessorUsagePropagationName<>(accessor, adapter));
	}

	public IVariable<IUsagePropagationName<Term, Edge>, TermRelation> of(Term term) {
		return new Variable<>(new TermUsagePropagationName<>(term));
	}

	public UsagePropagationBuilder<Term, Edge> set(Term term, TermRelation relation) {
		map.put(term, IFunction2.create(relation));
		return this;
	}

	public UsagePropagationBuilder<Term, Edge> with(IConsumer1<? super UsagePropagationBuilder<Term, Edge>> consumer) {
		consumer.accept(this);
		return this;
	}
}
