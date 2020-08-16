package com.g2forge.reassert.term.propogate;

import java.util.EnumMap;
import java.util.Map;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.term.StandardUsageTerm;
import com.g2forge.reassert.term.analyze.TermRelationBooleanSystem;
import com.g2forge.reassert.term.analyze.model.TermConstant;
import com.g2forge.reassert.term.analyze.model.logic.HTermLogic;
import com.g2forge.reassert.term.analyze.model.logic.ITermLogicContext;
import com.g2forge.reassert.term.eee.evaluate.IEvaluator;
import com.g2forge.reassert.term.eee.evaluate.bool.BooleanEvaluator;
import com.g2forge.reassert.term.eee.express.IExpression;

import lombok.AccessLevel;
import lombok.Getter;

public class UsageTermMapBuilder<E extends IEdge> implements IBuilder<Map<StandardUsageTerm, IFunction2<E, IUsage, TermRelation>>> {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IEvaluator<TermRelation, TermRelation> evaluator = new BooleanEvaluator<TermRelation>(TermRelationBooleanSystem.create());

	protected final Map<StandardUsageTerm, IFunction2<E, IUsage, TermRelation>> map = new EnumMap<>(StandardUsageTerm.class);

	@Override
	public Map<StandardUsageTerm, IFunction2<E, IUsage, TermRelation>> build() {
		return map;
	}

	public UsageTermMapBuilder<E> compute(StandardUsageTerm term, ITerm expression) {
		map.put(term, (d, u) -> {
			final IExpression<TermRelation> compiled = HTermLogic.getCompiler().apply(new ITermLogicContext() {
				@Override
				public Object getContext() {
					return d;
				}

				@Override
				public IExpression<TermRelation> term(ILicenseTerm licenseTerm) {
					throw new UnsupportedOperationException();
				}

				@Override
				public IExpression<TermRelation> term(IUsageTerm usageTerm) {
					return new TermConstant(usageTerm, u);
				}
			}, expression);
			return getEvaluator().eval(compiled);
		});
		return this;
	}

	public UsageTermMapBuilder<E> copy(StandardUsageTerm term) {
		map.put(term, (d, u) -> u.getTerms().getRelation(term));
		return this;
	}

	public UsageTermMapBuilder<E> exclude(StandardUsageTerm term) {
		return set(term, TermRelation.Excluded);
	}

	public UsageTermMapBuilder<E> include(StandardUsageTerm term) {
		return set(term, TermRelation.Included);
	}

	public UsageTermMapBuilder<E> set(StandardUsageTerm term, TermRelation relation) {
		map.put(term, (d, u) -> relation);
		return this;
	}
}