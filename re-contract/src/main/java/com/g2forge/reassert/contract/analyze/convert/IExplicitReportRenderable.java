package com.g2forge.reassert.contract.analyze.convert;

import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.reassert.contract.analyze.convert.IReportRenderContext;
import com.g2forge.reassert.contract.eee.explain.model.IExplained;

public interface IExplicitReportRenderable<T> extends IExplicitRenderable<IReportRenderContext>, IExplained<T> {}
