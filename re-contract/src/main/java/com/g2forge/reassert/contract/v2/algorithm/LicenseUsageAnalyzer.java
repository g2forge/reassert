package com.g2forge.reassert.contract.v2.algorithm;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.reassert.contract.model.findings.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.findings.UnrecognizedTermFinding;
import com.g2forge.reassert.contract.model.logic.ITermLogicContext;
import com.g2forge.reassert.contract.v2.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.v2.eval.TermRelationValueSystem;
import com.g2forge.reassert.contract.v2.model.ICTName;
import com.g2forge.reassert.contract.v2.model.rule.IRule;
import com.g2forge.reassert.contract.v2.model.rule.IRules;
import com.g2forge.reassert.core.model.contract.ILicenseUsageAnalyzer;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.express.v2.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class LicenseUsageAnalyzer implements ILicenseUsageAnalyzer {
	protected static interface IRecordingRuleContext extends ITermLogicContext {
		public Set<ITerm> getUsedTerms();
	}

	protected final IRules rules;

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	@Override
	public IReport report(IUsageApplied usageApplied, ILicenseApplied licenseApplied) {
		final IUsage usage = (IUsage) usageApplied;
		final ILicense license = (ILicense) licenseApplied;

		// Sets of usage terms we need to approve and license conditions we need to meet
		final Set<IUsageTerm> remainingUsageTerms = new LinkedHashSet<>(usage.getTerms().getTerms(true));
		final Set<ILicenseTerm> remainingLicenseConditions = license.getTerms().getTerms(true).stream().filter(term -> ILicenseTerm.Type.Condition.equals(term.getType())).collect(Collectors.toCollection(LinkedHashSet::new));

		final Report.ReportBuilder retVal = Report.builder();
		final AnalyzeTermExpressionEvaluator analyzer = new AnalyzeTermExpressionEvaluator(TermRelation.Included);
		final ExplainingEvaluator<ICTName, TermRelation> evaluator = new ExplainingEvaluator<>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());
		for (IRule rule : getRules().getRules()) {
			final IExpression<ICTName, TermRelation> expression = rule.getExpression();
			final AnalyzeTermExpressionEvaluator.Analyzed analyzed;
			final IExplained<TermRelation> explained;
			if (expression != null) {
				// Compute the input and output terms
				analyzed = analyzer.eval(expression);
				if (!analyzed.getInputs().containsAll(analyzed.getOutputs())) throw new IllegalArgumentException(String.format("Rule to satisfy \"%1$s\" did not use \"%2$s\"", analyzed.getOutputs().stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & ")), HCollection.difference(analyzed.getOutputs(), analyzed.getInputs()).stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & "))));

				// Evaluate the expression
				explained = evaluator.eval(expression);

				// Record that we handled the output terms
				remainingUsageTerms.removeAll(analyzed.getOutputs());
				remainingLicenseConditions.removeAll(analyzed.getOutputs());
			} else {
				analyzed = new AnalyzeTermExpressionEvaluator.Analyzed(null, HCollection.emptySet(), HCollection.emptySet());
				explained = new Literal<>(TermRelation.Included);
			}

			final IFinding finding = rule.getFinding().apply(explained);

			// Contextualize the finding
			retVal.finding(new ExpressionContextFinding(inputs, expression, outputs, finding));
		}

		// Report an error if any of the terms in our input weren't recognized
		remainingUsageTerms.stream().map(UnrecognizedTermFinding::new).forEach(retVal::finding);
		remainingLicenseConditions.stream().map(UnrecognizedTermFinding::new).forEach(retVal::finding);

		return retVal.build();
	}
}