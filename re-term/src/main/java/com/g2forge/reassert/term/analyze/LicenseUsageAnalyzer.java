package com.g2forge.reassert.term.analyze;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.ILicenseUsageAnalyzer;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.term.analyze.model.TermConstant;
import com.g2forge.reassert.term.analyze.model.findings.ExpressionContextualFinding;
import com.g2forge.reassert.term.analyze.model.findings.UnrecognizedTermFinding;
import com.g2forge.reassert.term.analyze.model.logic.ITermLogicContext;
import com.g2forge.reassert.term.analyze.model.rules.IRules;
import com.g2forge.reassert.term.analyze.model.rules.Rule;
import com.g2forge.reassert.term.eee.evaluate.bool.ExplainedBooleanEvaluator;
import com.g2forge.reassert.term.eee.explain.model.IExplained;
import com.g2forge.reassert.term.eee.express.IConstant;
import com.g2forge.reassert.term.eee.express.IExpression;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class LicenseUsageAnalyzer implements ILicenseUsageAnalyzer {
	protected static interface IRecordingRuleContext extends ITermLogicContext {
		public Set<ITerm> getUsedTerms();
	}

	@Getter
	@RequiredArgsConstructor
	protected static class RuleContextFactory {
		protected final IUsage usage;

		protected final ILicense license;

		protected final ExplainedBooleanEvaluator<TermRelation> evaluator = new ExplainedBooleanEvaluator<>(TermRelationBooleanSystem.create());

		public IRecordingRuleContext create() {
			return new IRecordingRuleContext() {
				@Getter
				protected final Set<ITerm> usedTerms = new LinkedHashSet<>();

				public IConstant<TermRelation> term(ILicenseTerm licenseTerm) {
					getUsedTerms().add(licenseTerm);
					return new TermConstant(licenseTerm, getLicense());
				}

				public IConstant<TermRelation> term(IUsageTerm usageTerm) {
					getUsedTerms().add(usageTerm);
					return new TermConstant(usageTerm, getUsage());
				}
			};
		}

	}

	protected final IRules ruleSet;

	@Override
	public IReport report(IUsage usage, ILicense license) {
		// Sets of usage terms we need to approve and license conditions we need to meet
		final Set<IUsageTerm> remainingUsageTerms = new LinkedHashSet<>(usage.getTerms().getSpecifiedTerms());
		final Set<ILicenseTerm> remainingLicenseConditions = license.getTerms().getSpecifiedTerms().stream().filter(term -> ILicenseTerm.Type.Condition.equals(term.getType())).collect(Collectors.toCollection(LinkedHashSet::new));

		final Report.ReportBuilder retVal = Report.builder();
		final RuleContextFactory contextFactory = new RuleContextFactory(usage, license);
		final ExplainedBooleanEvaluator<TermRelation> evaluator = new ExplainedBooleanEvaluator<>(TermRelationBooleanSystem.create());
		for (Rule rule : getRuleSet().getRules()) {
			// Evaluate the rule, and record all the terms it used
			final IRecordingRuleContext context = contextFactory.create();
			final Set<ITerm> outputs = rule.getSatisfied();

			final IFunction1<ITermLogicContext, IExpression<TermRelation>> expression = rule.getExpression();
			if (expression != null) {
				final IExpression<TermRelation> applied = expression.apply(context);

				// Compute the input and output terms
				final Set<ITerm> used = context.getUsedTerms();
				final Collection<ITerm> inputs = (outputs == null) ? used : HCollection.difference(used, outputs);
				if (!used.containsAll(outputs)) throw new IllegalArgumentException(String.format("Rule to satisfy \"%1$s\" did not use \"%2$s\"", outputs.stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & ")), HCollection.difference(outputs, used).stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & "))));

				// Evaluate the expression
				final IExplained<TermRelation> explained = evaluator.eval(applied);
				final IFinding finding = rule.getFinding().apply(explained);

				// Contextualize the finding
				retVal.finding(new ExpressionContextualFinding(inputs, applied, outputs, finding));
			}

			if (outputs != null) {
				remainingUsageTerms.removeAll(outputs);
				remainingLicenseConditions.removeAll(outputs);
			}
		}

		// Report an error if any of the terms in our input weren't recognized
		remainingUsageTerms.stream().map(UnrecognizedTermFinding::new).forEach(retVal::finding);
		remainingLicenseConditions.stream().map(UnrecognizedTermFinding::new).forEach(retVal::finding);

		return retVal.build();
	}
}