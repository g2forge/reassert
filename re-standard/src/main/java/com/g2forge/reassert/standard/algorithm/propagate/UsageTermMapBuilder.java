package com.g2forge.reassert.standard.algorithm.propagate;

import java.util.EnumMap;
import java.util.Map;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.TermRelationBooleanSystem;
import com.g2forge.reassert.contract.model.TermConstant;
import com.g2forge.reassert.contract.model.logic.HTermLogic;
import com.g2forge.reassert.contract.model.logic.ITermLogicContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.expression.evaluate.IEvaluator;
import com.g2forge.reassert.expression.evaluate.bool.BooleanEvaluator;
import com.g2forge.reassert.expression.express.IExpression;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

import lombok.AccessLevel;
import lombok.Getter;

public class UsageTermMapBuilder<E extends IEdge> implements IBuilder<Map<StandardUsageTerm, IFunction2<E, IUsageApplied, TermRelation>>> {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IEvaluator<TermRelation, TermRelation> evaluator = new BooleanEvaluator<TermRelation>(TermRelationBooleanSystem.create());

	protected final Map<StandardUsageTerm, IFunction2<E, IUsageApplied, TermRelation>> map = new EnumMap<>(StandardUsageTerm.class);

	@Override
	public Map<StandardUsageTerm, IFunction2<E, IUsageApplied, TermRelation>> build() {
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

				@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
				@Override
				public IExpression<TermRelation> term(IUsageTerm usageTerm) {
					return new TermConstant(usageTerm, (IUsage) u);
				}
			}, expression);
			return getEvaluator().eval(compiled);
		});
		return this;
	}

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	public UsageTermMapBuilder<E> copy(StandardUsageTerm term) {
		map.put(term, (d, u) -> ((IUsage) u).getTerms().getRelation(term));
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