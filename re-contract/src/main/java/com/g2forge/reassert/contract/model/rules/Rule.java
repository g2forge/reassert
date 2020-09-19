package com.g2forge.reassert.contract.model.rules;

import java.util.Set;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.model.logic.HTermLogic;
import com.g2forge.reassert.contract.model.logic.ITermLogicContext;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.explain.model.IExplained;
import com.g2forge.reassert.express.express.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Rule {
	public static class RuleBuilder implements IBuilder<Rule> {
		public RuleBuilder expression$(ITerm term) {
			expression(context -> HTermLogic.getCompiler().apply(context, term));
			return this;
		}
	}

	protected final IFunction1<ITermLogicContext, IExpression<TermRelation>> expression;

	protected final IFunction1<IExplained<TermRelation>, IFinding> finding;

	@Singular("satisfied")
	protected final Set<ITerm> satisfied;
}
