// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="Overview.html"><strong aria-hidden="true">1.</strong> Overview</a></li><li class="chapter-item expanded "><a href="Introduction.html"><strong aria-hidden="true">2.</strong> Introduction</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Introduction/DelayAndForce.html"><strong aria-hidden="true">2.1.</strong> Delay and Force</a></li><li class="chapter-item expanded "><a href="Introduction/PatternMatching.html"><strong aria-hidden="true">2.2.</strong> Pattern Matching</a></li><li class="chapter-item expanded "><a href="Introduction/PlutarchTerms.html"><strong aria-hidden="true">2.3.</strong> Plutarch Terms</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Introduction/PlutarchTerms/PlutarchConstants.html"><strong aria-hidden="true">2.3.1.</strong> Plutarch Constants</a></li><li class="chapter-item expanded "><a href="Introduction/PlutarchTerms/PlutarchLambdas.html"><strong aria-hidden="true">2.3.2.</strong> Plutarch Lambdas</a></li></ol></li><li class="chapter-item expanded "><a href="Introduction/PlutarchTypes.html"><strong aria-hidden="true">2.4.</strong> Plutarch Types</a></li><li class="chapter-item expanded "><a href="Introduction/UntypedPlutusCore.html"><strong aria-hidden="true">2.5.</strong> Untyped PlutusCore</a></li><li class="chapter-item expanded "><a href="DEVGUIDE.html"><strong aria-hidden="true">2.6.</strong> Dev Guide</a></li><li class="chapter-item expanded "><a href="Run.html"><strong aria-hidden="true">2.7.</strong> Run</a></li><li class="chapter-item expanded "><a href="Troubleshooting.html"><strong aria-hidden="true">2.8.</strong> Troubleshooting</a></li></ol></li><li class="chapter-item expanded "><div><strong aria-hidden="true">3.</strong> Examples</div></li><li><ol class="section"><li class="chapter-item expanded "><a href="examples/BASIC.html"><strong aria-hidden="true">3.1.</strong> Basic</a></li><li class="chapter-item expanded "><a href="examples/VALIDATOR.html"><strong aria-hidden="true">3.2.</strong> Validator</a></li></ol></li><li class="chapter-item expanded "><a href="Concepts.html"><strong aria-hidden="true">4.</strong> Concepts</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Concepts/DataAndScottEncoding.html"><strong aria-hidden="true">4.1.</strong> Data and Scott Encoding</a></li><li class="chapter-item expanded "><a href="Concepts/GenericProgramming.html"><strong aria-hidden="true">4.2.</strong> Generic Programming</a></li><li class="chapter-item expanded "><a href="Concepts/HaskellSynonym.html"><strong aria-hidden="true">4.3.</strong> Haskell Synonym</a></li><li class="chapter-item expanded "><a href="Concepts/Hoisting.html"><strong aria-hidden="true">4.4.</strong> Hoisting</a></li><li class="chapter-item expanded "><a href="Concepts/WhatIsTheS.html"><strong aria-hidden="true">4.5.</strong> What is the s?</a></li></ol></li><li class="chapter-item expanded "><a href="Types.html"><strong aria-hidden="true">5.</strong> Types</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Types/PAsData.html"><strong aria-hidden="true">5.1.</strong> PAsData</a></li><li class="chapter-item expanded "><a href="Types/PBool.html"><strong aria-hidden="true">5.2.</strong> PBool</a></li><li class="chapter-item expanded "><a href="Types/PBuiltinList.html"><strong aria-hidden="true">5.3.</strong> PBuiltinList</a></li><li class="chapter-item expanded "><a href="Types/PBuiltinPair.html"><strong aria-hidden="true">5.4.</strong> PBuiltinPair</a></li><li class="chapter-item expanded "><a href="Types/PByteString.html"><strong aria-hidden="true">5.5.</strong> PByteString</a></li><li class="chapter-item expanded "><a href="Types/PData.html"><strong aria-hidden="true">5.6.</strong> PData</a></li><li class="chapter-item expanded "><a href="Types/PDataSumAndPDataRecord.html"><strong aria-hidden="true">5.7.</strong> PDataSum and PDataRecord</a></li><li class="chapter-item expanded "><a href="Types/PInteger.html"><strong aria-hidden="true">5.8.</strong> PInteger</a></li><li class="chapter-item expanded "><a href="Types/PList.html"><strong aria-hidden="true">5.9.</strong> PList</a></li><li class="chapter-item expanded "><a href="Types/PString.html"><strong aria-hidden="true">5.10.</strong> PString</a></li><li class="chapter-item expanded "><a href="Types/PUnit.html"><strong aria-hidden="true">5.11.</strong> PUnit</a></li></ol></li><li class="chapter-item expanded "><a href="Typeclasses.html"><strong aria-hidden="true">6.</strong> Typeclasses</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Typeclasses/PLiftable.html"><strong aria-hidden="true">6.1.</strong> PLiftable</a></li><li class="chapter-item expanded "><a href="Typeclasses/PEqAndPOrd.html"><strong aria-hidden="true">6.2.</strong> PEq and POrd</a></li><li class="chapter-item expanded "><a href="Typeclasses/PIntegral.html"><strong aria-hidden="true">6.3.</strong> PIntegral</a></li><li class="chapter-item expanded "><a href="Typeclasses/PIsData.html"><strong aria-hidden="true">6.4.</strong> PIsData</a></li><li class="chapter-item expanded "><a href="Typeclasses/PIsDataReprAndPDataFields.html"><strong aria-hidden="true">6.5.</strong> PIsDataRepr and PDataFields</a></li><li class="chapter-item expanded "><a href="Typeclasses/PListLike.html"><strong aria-hidden="true">6.6.</strong> PListLike</a></li><li class="chapter-item expanded "><a href="Typeclasses/PlutusType,PCon,PMatch.html"><strong aria-hidden="true">6.7.</strong> PlutusType, PCon, and PMatch</a></li><li class="chapter-item expanded "><a href="Typeclasses/PTryFrom.html"><strong aria-hidden="true">6.8.</strong> PTryFrom</a></li></ol></li><li class="chapter-item expanded "><a href="Usage.html"><strong aria-hidden="true">7.</strong> Usage</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Usage/AvoidWorkDuplicationUsingPlet.html"><strong aria-hidden="true">7.1.</strong> Avoid Work Duplication Using plet</a></li><li class="chapter-item expanded "><a href="Usage/Conditionals.html"><strong aria-hidden="true">7.2.</strong> Conditionals</a></li><li class="chapter-item expanded "><a href="Usage/DerivingForNewtypes.html"><strong aria-hidden="true">7.3.</strong> Deriving for Newtypes</a></li><li class="chapter-item expanded "><a href="Usage/DerivingWithGenerics.html"><strong aria-hidden="true">7.4.</strong> Deriving with Generics</a></li><li class="chapter-item expanded "><a href="Usage/DoSyntaxWithQualifiedDo.html"><strong aria-hidden="true">7.5.</strong> DoSyntax with QualifiedDo</a></li><li class="chapter-item expanded "><a href="Usage/DoSyntaxWithTermCont.html"><strong aria-hidden="true">7.6.</strong> DoSyntax with TermCont</a></li><li class="chapter-item expanded "><a href="Usage/RaisingErrors.html"><strong aria-hidden="true">7.7.</strong> Raising Errors</a></li><li class="chapter-item expanded "><a href="Usage/Recursion.html"><strong aria-hidden="true">7.8.</strong> Recursion</a></li><li class="chapter-item expanded "><a href="Usage/Tracing.html"><strong aria-hidden="true">7.9.</strong> Tracing</a></li><li class="chapter-item expanded "><a href="Usage/UnsafeFunctions.html"><strong aria-hidden="true">7.10.</strong> Unsafe Functions</a></li></ol></li><li class="chapter-item expanded "><a href="Tricks.html"><strong aria-hidden="true">8.</strong> Tricks</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="Tricks/DifferenceBetweenPconAndPconstant.html"><strong aria-hidden="true">8.1.</strong> Difference between pcon and pconstant</a></li><li class="chapter-item expanded "><a href="Tricks/DontDuplicateWork.html"><strong aria-hidden="true">8.2.</strong> Don&#39;t Duplicate Work</a></li><li class="chapter-item expanded "><a href="Tricks/makeIsDataIndexed,HaskellADTs,PIsDataRepr.html"><strong aria-hidden="true">8.3.</strong> makeIsDataIndexed, HaskellADTs, and PIsDataRepr</a></li><li class="chapter-item expanded "><a href="Tricks/OptimizingUnhoistableLambdas.html"><strong aria-hidden="true">8.4.</strong> Optimizing Unhoistable Lambdas</a></li><li class="chapter-item expanded "><a href="Tricks/PlutarchFunctionsStrict.html"><strong aria-hidden="true">8.5.</strong> Plutarch Functions Strict</a></li><li class="chapter-item expanded "><a href="Tricks/PreferMatchingOnPmatchResultImmediately.html"><strong aria-hidden="true">8.6.</strong> Prefer Matching on pmatch Result Immediately</a></li><li class="chapter-item expanded "><a href="Tricks/PreferPlutarchFunctions.html"><strong aria-hidden="true">8.7.</strong> Prefer Plutarch Functions</a></li><li class="chapter-item expanded "><a href="Tricks/PreferStaticallyBuildingConstants.html"><strong aria-hidden="true">8.8.</strong> Prefer Statically Building Constants</a></li><li class="chapter-item expanded "><a href="Tricks/RepresentationOfPlutarchType.html"><strong aria-hidden="true">8.9.</strong> Representation of Plutarch Type</a></li><li class="chapter-item expanded "><a href="Tricks/ResponsibilityOfEvaluationInHaskellFunctions.html"><strong aria-hidden="true">8.10.</strong> Responsibility of Evaluation in Haskell Functions</a></li><li class="chapter-item expanded "><a href="Tricks/UsingHaskellLevelFunctions.html"><strong aria-hidden="true">8.11.</strong> Using Haskell Level Functions</a></li><li class="chapter-item expanded "><a href="Tricks/WorkingWithBoundFields.html"><strong aria-hidden="true">8.12.</strong> Working with Bound Fields</a></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString();
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
